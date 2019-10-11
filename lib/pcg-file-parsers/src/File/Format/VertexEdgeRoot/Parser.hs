-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.VertexEdgeRoot.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing VER files into a collection of the
-- vertex set, edge set, and root set representing a "Phylogenetic Forest".
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module File.Format.VertexEdgeRoot.Parser
  ( VertexLabel
  , EdgeLength
  , VertexSetType(..)
  , EdgeInfo(..)
  , VertexEdgeRoot(..)
  , connectedVertex
  , verStreamParser
  , labeledVertexSetDefinition
  , vertexSetType
  , unlabeledVertexSetDefinition
  , vertexLabelDefinition
  , edgeSetDefinition
  , edgeDefinition
  ) where

import           Control.DeepSeq        (NFData)
import           Control.Monad.Fail
import           Data.CaseInsensitive   (FoldCase)
import           Data.Char              (isSpace)
import           Data.Data
import           Data.Either            (partitionEithers)
import           Data.Foldable
import           Data.Functor           (($>))
import           Data.Key
import           Data.List              (intercalate, partition)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NE
import           Data.List.Utility      (duplicates)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           Data.Monoid
import           Data.Ord               (comparing)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)
import           Prelude                hiding (lookup)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- A textual identifier for a node in the graph
type  VertexLabel   = String


-- |
-- The possibly calculated distance between two nodes in the graph
type  EdgeLength    = Maybe Double


-- |
-- The two types of sets of nodes present in a VER file
data  VertexSetType = Vertices | Roots deriving (Eq, Generic, NFData, Show)


-- |
-- Connection between two nodes in the graph along with the distance of the
-- connection. Edges are interpred as bidirectional when read in and given
-- direction when interpreted relative to the root of the tree.
data  EdgeInfo
    = EdgeInfo
    { edgeOrigin :: VertexLabel -- ^ Extract the origin of the directed edge
    , edgeTarget :: VertexLabel -- ^ Extract the destination of the directed edge
    , edgeLength :: EdgeLength  -- ^ Extract the /possibly/ present edge length
    }
    deriving stock (Data, Eq, Generic, Ord, Typeable)
    deriving anyclass (NFData)


-- |
-- Collection of Vericies, roots, and edges representing a "Phylogenetic Forest"
data  VertexEdgeRoot
    = VER
    { vertices :: Set VertexLabel
    , edges    :: Set EdgeInfo
    , roots    :: Set VertexLabel
    }
    deriving stock    (Data, Eq, Generic, Show, Typeable)
    deriving anyclass (NFData)


-- | (✔)
instance Show EdgeInfo where
  show (EdgeInfo x y c) = fold $ ["(", show x, ", ", show y, ")"] <> renderCost c
    where
      renderCost = fmap ((":" <>) . show) . toList


-- |
-- For a given vertex, attempts to get the connected vertex from the 'EdgeInfo'.
-- If the input vertex was present in the 'EdgeInfo', returns 'Just v' where
-- @v@ is the corresponsing 'VertexLabel'. If the input vertex was not present
-- in the 'EdgeInfo'.
connectedVertex :: VertexLabel -> EdgeInfo -> Maybe VertexLabel
connectedVertex v (EdgeInfo a b _)
  | v == a    = Just b
  | v == b    = Just a
  | otherwise = Nothing


-- |
-- Reads two vertex sets and an edge set, conditionally infers the root set
-- when vertex sets are unlabeled. Ensures that the elements of the root set
-- are not connected in the forest. Ensures that the rooted trees in the
-- forest do not contain cycles.
verStreamParser :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
verStreamParser = validateForest =<< verDefinition


-- We have a complicated definition here because we do not want to restrict
-- the order of the set definitions, and yet we must enforce that there is
-- only one edge set and two vertex sets. One vertex set is the set of all
-- verticies and the other is a subset consisting of the root nodes. To
-- enforce this for propper parsing, and provide robust error messages we
-- read zero or more set definitions and seperate each set as either a vertex
-- set or an edge set by checking the type constructor for a Left or Right
-- value. We then assert that we have received exactly one edge set and
-- exactly two vertex sets. If not we generate meaningful error messages based
-- on the missing or multiple requisite sets. Once all sets have been parsed
-- we disambiguate the vertex sets to the set of verticies and the set of root
-- nodes by inspecting the possibly provided set labels or in the absence of
-- labels by comparing the size of the sets; as the set of all verticies is
-- surely a superset of the set of root nodes.
-- |
-- Parses exactly one vertex set, one edge set, and one root set.
verDefinition :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
verDefinition = do
    sets <- many setDefinition
    case partitionEithers sets of
      ([edges'], [x,y]) -> formVertexEdgeRoot x y edges'
      (xs      , ys   ) -> runFail $ edgeSetMessages xs <> vertexSetMessages ys
  where
    formVertexEdgeRoot x@(typeA, setA) y@(typeB, setB) edges' =
      case (typeA, typeB) of
        (Nothing       , Nothing      ) -> let -- [m,n] = sortBy (comparing length) [setA,setB]
                                               m = minimumBy (comparing length) [setA, setB]
                                               n = maximumBy (comparing length) [setA, setB]
                                           in pure $ VER n    edges' m
        (Nothing       , Just Vertices) ->    pure $ VER setB edges' setA
        (Nothing       , Just Roots   ) ->    pure $ VER setA edges' setB
        (Just Vertices , Nothing      ) ->    pure $ VER setA edges' setB
        (Just Roots    , Nothing      ) ->    pure $ VER setB edges' setA
        (Just Vertices , Just Roots   ) ->    pure $ VER setA edges' setB
        (Just Roots    , Just Vertices) ->    pure $ VER setB edges' setA
        (_             , _            ) -> runFail $ vertexSetMessages [x,y]
    runFail [x] = fail x
    runFail xs  = fails xs
    vertexSetMessages xs    = rootSetMessages roots' <> vertSetMessages verticies'
      where
        (roots',verticies') = partition isRoot xs
        isRoot              = (Just Roots ==) . fst
    edgeSetMessages         = messages "edge set"
    rootSetMessages         = messages "root set"
    vertSetMessages         = messages "vertex set"
    messages name []      = [message "No" name]
    messages _    [_]     = []
    messages name (_:_:_) = [message "Multiple" (name<>"s")]
    message x y             = concat [x," ",y," defined in input"]


-- |
-- We read a set from the input. The set can be an edge set or a vertex set.
-- If it is a vertex set, it may be labeled as a specific set of verticies or
-- a set of roots. We use the Either type as a return type to denote the
-- conditional type of the result.
setDefinition :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Either (Set EdgeInfo) (Maybe VertexSetType, Set VertexLabel))
setDefinition = do
    result <- optional (try edgeSetDefinition)
    case result of
      Just x  -> pure $ Left x
      Nothing -> Right <$> vertexSetDefinition


-- |
-- A vertex set can be labeled or unlabeled. We first attempt to read in a
-- labeled vertex set, and if that fails an unlabeled vertex set. The label
-- is returned contidionally in a Maybe type.
vertexSetDefinition :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Maybe VertexSetType, Set VertexLabel)
vertexSetDefinition = try labeledVertexSetDefinition <|> unlabeledVertexSetDefinition


-- |
-- A labeled vertex set contains a label followed by an unlabeled vertex set
labeledVertexSetDefinition :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Maybe VertexSetType, Set VertexLabel )
labeledVertexSetDefinition = do
    setType <- symbol vertexSetType
    _       <- symbol (char '=')
    (_,set) <- unlabeledVertexSetDefinition
    pure (Just setType, set)


-- |
-- A vertex set label is one of the following case insensative strings:
-- "vertexset", rootset"
vertexSetType :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m VertexSetType
vertexSetType = do
    value <- optional . try . symbol $ string'' "VertexSet"
    case value of
      Just _  -> pure Vertices
      Nothing -> symbol (string'' "RootSet") $> Roots


-- |
-- A vertex set with an optional set label enclosed in braces. A vertex set
-- cannot have duplicate verticies.
unlabeledVertexSetDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Maybe VertexSetType, Set VertexLabel)
unlabeledVertexSetDefinition = validateVertexSet =<< unlabeledVertexSetDefinition'
  where
    unlabeledVertexSetDefinition' :: (MonadParsec e s m, Token s ~ Char) => m (Maybe VertexSetType, [VertexLabel])
    unlabeledVertexSetDefinition' = do
        _       <- symbol (char '{')
        labels' <- symbol $ vertexLabelDefinition `sepBy1` try (symbol (char ','))
        _       <- symbol (char '}')
        pure (Nothing, labels')
--    validateVertexSet :: (MonadParsec e s m, Token s ~ Char) => (Maybe VertexSetType, [VertexLabel]) -> m (Maybe VertexSetType, Set VertexLabel)
    validateVertexSet (t,vs)
      | null dupes = pure (t, Set.fromList vs)
      | otherwise  = fail errorMessage
      where
        dupes = duplicates vs
        errorMessage = "The following verticies were defined multiple times: " <> show dupes


-- |
-- A vertex label is any non-scpace character that is also not a brace, paren,
-- or comma.
vertexLabelDefinition :: (MonadParsec e s m, Token s ~ Char) => m String
vertexLabelDefinition = some validChar
  where
    validChar = satisfy $ \x -> x `notElem` "{}()," && (not . isSpace) x


-- |
-- Parses an edge set with an optional edge set label.
-- Edges cannot be from one node to the same node.
-- Edges are undirected, with duplicate edges prohibited.
edgeSetDefinition :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m (Set EdgeInfo)
edgeSetDefinition = validateEdgeSet =<< edgeSetDefinition'
  where
--    edgeSetLabel :: (MonadParsec e s m, Token s ~ Char) => m String
    edgeSetLabel = symbol (string'' "EdgeSet") <* symbol (char '=')

--    edgeSetDefinition' :: (MonadParsec e s m, Token s ~ Char) => m [EdgeInfo]
    edgeSetDefinition' = do
        _      <- optional (try edgeSetLabel)
        _      <- symbol (char '{')
        pairs  <- edgeDefinition `sepBy` symbol (char ',')
        _      <- symbol (char '}')
        pure pairs

--    validateEdgeSet :: (MonadParsec e s m, Token s ~ Char) => [EdgeInfo] -> m (Set EdgeInfo)
    validateEdgeSet es
      | null errors = pure $ Set.fromList es
      | otherwise   = fails errors
      where
        edges' = toTuple <$> es
        dupes  = duplicates edges'
        selfs  = filter (uncurry (==)) edges'
        biDirs :: [(EdgeInfo, EdgeInfo)]
        biDirs = filter (uncurry isReflexive) [(x,y) | x <- es, y <- es, x /= y ]
        errors = snd <$> filter (not . fst)
            [ (null  dupes,         dupesErrorMessage)
            , (null  selfs,         selfsErrorMessage)
            , (null biDirs, biDirectionalErrorMessage)
            ]
          where
            dupesErrorMessage = "Duplicate edges detected. The following edges were defined multiple times: "    <> show dupes
            selfsErrorMessage = "Self-referencing edge(s) detected.The following edge(s) are self-referencing: " <> show selfs
            biDirectionalErrorMessage = "One or more bidirectional edges detected: " <> biDirectionShow biDirs
            biDirectionShow :: [(EdgeInfo, EdgeInfo)] -> String
            biDirectionShow = (\x -> "[" <> x <> "]") . intercalate ", " . fmap g
              where
                g (lhs, rhs) = show lhs <> " <--> " <> show rhs


-- |
-- Converts EdgeInfo to an tuple of VertexLabels, representing edge direction.
toTuple :: EdgeInfo -> (VertexLabel, VertexLabel)
toTuple (EdgeInfo x y _) = (x, y)


-- |
-- Determine if two edges satisfy the reflexive relation.
isReflexive :: EdgeInfo -> EdgeInfo -> Bool
isReflexive (EdgeInfo a b _) (EdgeInfo x y _) = a == y && b == x



-- |
-- Defines the serialized format of an edge connecting nodes @a@ and @b@ as
-- '"(a,b)"'. Allows for optional "branch length" annotation as '"(a,b):12.34"'.
edgeDefinition :: (MonadParsec e s m, Token s ~ Char) => m EdgeInfo
edgeDefinition = symbol $ do
    _ <- space
    _ <- symbol (char '(')
    x <- symbol vertexLabelDefinition
    _ <- symbol (char ',')
    y <- symbol vertexLabelDefinition
    _ <- symbol (char ')')
    z <- optional $ try branchLengthDefinition
    pure $ EdgeInfo x y z
  where
    branchLengthDefinition = symbol (char ':') *> symbol double


-- |
-- Convinence combinator to consume trailing whitespace.
symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol x = x <* space


-- |
-- Validates a parse result to ensure that the resulting forest is internally
-- consistent.
--
-- A VER forest is not consistent if:
--
--   * Any spcified root node has in-degree greater than 0
--
--   * A directed cycle exists
validateForest :: (MonadParsec e s m {- , Token s ~ Char -}) => VertexEdgeRoot -> m VertexEdgeRoot
validateForest ver@(VER vs es rs ) =
    case errors of
      [] -> pure ver
      xs -> fails xs
  where
    rootList       = toList rs
    connections    = buildEdgeMap vs es
    errors         = badRootErrorMessages <> cycleErrorMessages

    -- |
    badRoots :: Map VertexLabel (NonEmpty VertexLabel)
    badRoots       = foldMap f rs
      where
        f rootLabel =
            case foldMapWithKey g connections of
              [] -> mempty
              xs -> Map.singleton rootLabel $ NE.fromList xs
          where
            g k v
              | rootLabel `elem` v = [k]
              | otherwise          = []

    -- |
    -- Detect if the tree contains a cycle by consulting a stack
    -- while performing a depth-first-search
    treeEdgeCycles = foldMap mergeCycles [ (x,y) | x <- resultList, y <- resultList, x <= y ]
      where
        resultList = catMaybes $ findCycle <$> rootList

        -- We merge cycles to provide nice error messages when a netowrk has multiple roots.
        mergeCycles ((r1,cycle1), (r2,cycle2))
          | cycle1 == cycle2 && r1 /= r2 = [(r1:|[r2], cycle1)]
          | otherwise                    = [(r1:|[]  , cycle1), (r2:|[], cycle2)]

        findCycle :: VertexLabel -> Maybe (VertexLabel, NonEmpty VertexLabel)
        findCycle root =
            case result of
              [] -> Nothing
              xs -> Just (root, NE.fromList xs)
          where
            result = findCycle' [] root
            findCycle' stack node
              | cycleDetected = cycle'
              | null children = []
              | otherwise     = maximumBy (comparing length) childCycles
              where
                (inner, base) = span (/= node) stack
                cycleDetected = not $ null base
                cycle'        = [node] <> reverse inner <> [node]
                childCycles   = findCycle' (node:stack) <$> toList children
                children      = fold $ node `lookup` connections

{-
    -- Determine if multiple roots are connected by traversing the tree
    -- and checking each node for inclusion in the root set.
    findRoots :: VertexLabel -> [VertexLabel]
    findRoots root = findRoots' root root
      where
        findRoots' parent node = selfRoot <> descendantRoots
          where
            selfRoot        = filter (`elem` rs) [node]
            descendantRoots = concat $ findRoots' node <$> children
            children        = parent `delete` adjacentNodes
            adjacentNodes   = fromMaybe [] $ node `lookup` connections

    manyRootsErrorMessage xs = concat
      [ "Multiple root nodes detected in a single tree. "
      , "The following root nodes should form different trees, but thay are part of the same tree: "
      , show xs
      ]
-}

    badRootErrorMessages :: [String]
    badRootErrorMessages = foldMapWithKey f badRoots
      where
        f k v = pure $ fold
            [ "Root node cannot have parents! For specified root node '"
            , show k
            , "' the following parent nodes were found: "
            , show $ toList v
            ]

    cycleErrorMessages :: [String]
    cycleErrorMessages = edgeCycleErrorMessage <$> treeEdgeCycles
      where
        edgeCycleErrorMessage (r,xs) = concat
            [ "In the tree rooted at "
            , shownRoots
            , ", the following cycle was detected: "
            , show xs
            ]
          where
            shownRoots
              | length r == 1 = "'" <> show r <> "'"
              | otherwise     = show $ toList r



-- |
-- Convience method for building a connection 'Map' based on the existing edges
-- in the graph.
buildEdgeMap :: Set VertexLabel -> Set EdgeInfo -> Map VertexLabel (Set VertexLabel)
buildEdgeMap vs es = foldMap buildMap vs
  where
    buildMap nodeLabel = foldl' f mempty es
      where
        f m e
          | edgeOrigin e == nodeLabel = Map.insertWith (<>) nodeLabel (Set.singleton $ edgeTarget e) m
          | otherwise                 = m

