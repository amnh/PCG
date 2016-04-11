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

{-# LANGUAGE FlexibleContexts #-}

module File.Format.VertexEdgeRoot.Parser where

import Data.Char              (isSpace)
import Data.Functor           (($>))
import Data.Either            (partitionEithers)
import Data.List              (delete,partition,maximumBy,sortBy)
import Data.List.Utility      (duplicates)
import Data.Map               (Map,empty,insert,lookup)
import Data.Maybe             (catMaybes,fromMaybe)
import Data.Ord               (comparing)
import Data.Set               (Set,elems,fromList,size)
import Prelude         hiding (lookup)
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim   (MonadParsec)

-- | A textual identifier for a node in the graph
type VertexLabel   = String

-- | The possibly calculated distance between two nodes in the graph
type EdgeLength    = Maybe Double

-- | The two types of sets of nodes present in a VER file
data VertexSetType = Vertices | Roots deriving (Eq,Show)

-- | Connection between two nodes in the graph along with the distance of the connection
--   Edges are interpred as bidirectional when read in and given direction when interpreted
--   relative to the root of the tree.
data EdgeInfo      = EdgeInfo (VertexLabel,VertexLabel) EdgeLength deriving (Show,Eq,Ord)

-- | Collection of Vericies, roots, and edges representing a "Phylogenetic Forest"
data VertexEdgeRoot
   = VER
   { vertices :: Set VertexLabel
   , edges    :: Set EdgeInfo
   , roots    :: Set VertexLabel
   } deriving (Show, Eq)

-- | Returns the `EdgeInfo as a tuple of 'VertexLabel's satisfying the constraint:
--
-- > let (a,b) = edgeConnection e in a <= b
edgeConnection :: EdgeInfo -> (VertexLabel,VertexLabel)
edgeConnection (EdgeInfo (a,b) _)
  | a <= b    = (a,b)
  | otherwise = (b,a)

edgeOrigin :: EdgeInfo -> VertexLabel
edgeOrigin (EdgeInfo (e,_) _) = e

edgeTarget :: EdgeInfo -> VertexLabel
edgeTarget (EdgeInfo (_,e) _) = e

edgeLength :: EdgeInfo -> Maybe Double
edgeLength (EdgeInfo (_,_) n) = n
                               
-- | Reads two vertex sets and an edge set, conditionally infers the root set
-- when vertex sets are unlabeled. Ensures that the elements of the root set
-- are not connected in the forest. Ensures that the rooted trees in the
-- forest do not contain cycles.
verStreamParser :: MonadParsec s m Char => m VertexEdgeRoot
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
-- | Parses exactly one vertex set, one edge set, and one root set.
verDefinition :: MonadParsec s m Char => m VertexEdgeRoot
verDefinition = do
    sets <- many setDefinition
    case partitionEithers sets of
      ([edges'], [x,y]) -> formVertexEdgeRoot x y edges'
      (xs      , ys   ) -> runFail $ edgeSetMessages xs ++ vertexSetMessages ys
  where
    formVertexEdgeRoot x@(typeA, setA) y@(typeB, setB) edges' =
      case (typeA, typeB) of
        (Nothing       , Nothing      ) -> let [m,n] = sortBy (comparing size) [setA,setB]
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
    vertexSetMessages xs    = rootSetMessages roots' ++ vertSetMessages verticies'
      where
        (roots',verticies') = partition isRoot xs
        isRoot              = (Just Roots ==) . fst
    edgeSetMessages         = messages "edge set"
    rootSetMessages         = messages "root set"
    vertSetMessages         = messages "vertex set"
    messages name []        = [message "No" name]
    messages _    [_]       = []
    messages name (_:_:_)   = [message "Multiple" (name++"s")]
    message x y             = concat [x," ",y," defined in input"]

-- | We read a set from the input. The set can be an edge set or a vertex set.
-- If it is a vertex set, it may be labeled as a specific set of verticies or
-- a set of roots. We use the Either type as a return type to denote the 
-- conditional type of the result.
setDefinition :: MonadParsec s m Char => m (Either (Set EdgeInfo) (Maybe VertexSetType, Set VertexLabel))
setDefinition = do
    result <- optional (try edgeSetDefinition)
    case result of
      Just x  -> pure $ Left x
      Nothing -> Right <$> vertexSetDefinition

-- | A vertex set can be labeled or unlabeled. We first attempt to read in a 
-- labeled vertex set, and if that fails an unlabeled vertex set. The label
-- is returned contidionally in a Maybe type.
vertexSetDefinition :: MonadParsec s m Char => m (Maybe VertexSetType, Set VertexLabel)
vertexSetDefinition = try labeledVertexSetDefinition <|> unlabeledVertexSetDefinition

-- | A labeled vertex set contains a label followed by an unlabeled vertex set
labeledVertexSetDefinition :: MonadParsec s m Char => m (Maybe VertexSetType, Set VertexLabel )
labeledVertexSetDefinition = do
    setType <- vertexSetType
    _       <- symbol (char '=')
    (_,set) <- unlabeledVertexSetDefinition
    pure (Just setType, set)

-- | A vertex set label is one of the following case insensative strings:
-- "vertexset", rootset"
vertexSetType :: MonadParsec s m Char => m VertexSetType
vertexSetType = do
    value <- optional (try (symbol (string' "VertexSet")))
    case value of
      Just _  -> pure Vertices
      Nothing -> symbol (string' "RootSet") $> Roots

-- | A vertex set with an optional set label enclosed in braces.
-- A vertex set cannot have duplicate verticies
unlabeledVertexSetDefinition :: MonadParsec s m Char => m (Maybe VertexSetType, Set VertexLabel)
unlabeledVertexSetDefinition = validateVertexSet =<< unlabeledVertexSetDefinition'
  where
    unlabeledVertexSetDefinition' :: MonadParsec s m Char => m (Maybe VertexSetType, [VertexLabel])
    unlabeledVertexSetDefinition' = do
        _       <- symbol (char '{')
        labels' <- vertexLabelDefinition `sepBy1` symbol (char ',')
        _       <- symbol (char '}')
        pure (Nothing, labels')
    validateVertexSet :: MonadParsec s m Char => (Maybe VertexSetType, [VertexLabel]) -> m (Maybe VertexSetType, Set VertexLabel)
    validateVertexSet (t,vs)
      | null dupes = pure (t, fromList vs)
      | otherwise  = fail errorMessage
      where
        dupes = duplicates vs
        errorMessage = "The following verticies were defined multiple times: " ++ show dupes

-- | A vertex label is any non-scpace character that is also not a brace, paren, or comma.
vertexLabelDefinition :: MonadParsec s m Char => m String
vertexLabelDefinition = some validChar
  where
    validChar = satisfy $ \x -> x `notElem` "{}(),"
                             && (not . isSpace) x

-- | Parses an edge set with an optional edge set label.
-- Edges cannot be from one node to the same node.
-- Edges are undirected, with duplicate edges prohibited.
edgeSetDefinition :: MonadParsec s m Char => m (Set EdgeInfo)
edgeSetDefinition = validateEdgeSet =<< edgeSetDefinition'
  where
    edgeSetLabel :: MonadParsec s m Char => m String
    edgeSetLabel = symbol (string' "EdgeSet") <* symbol (char '=')

    edgeSetDefinition' :: MonadParsec s m Char => m [EdgeInfo]
    edgeSetDefinition' = do
        _      <- optional (try edgeSetLabel)
        _      <- symbol (char '{')
        pairs  <- edgeDefinition `sepBy` symbol (char ',')
        _      <- symbol (char '}')
        pure pairs

    validateEdgeSet :: MonadParsec s m Char => [EdgeInfo] -> m (Set EdgeInfo)
    validateEdgeSet es
      | null errors = pure $ fromList es
      | otherwise   = fails errors
      where
        edges' = edgeConnection <$> es
        dupes  = duplicates edges'
        selfs  = filter (uncurry (==)) edges'
        errors = case (dupes,selfs) of
                   ([] ,[] ) -> []
                   (_:_,[] ) -> [dupesErrorMessage]
                   ([] ,_:_) -> [selfsErrorMessage] 
                   (_:_,_:_) -> [dupesErrorMessage,selfsErrorMessage]
        dupesErrorMessage = "Duplicate edges detected. The following edges were defined multiple times: "    ++ show dupes
        selfsErrorMessage = "Self-referencing edge(s) detected.The following edge(s) are self=referencing: " ++ show selfs

-- | Defines the serialized format of an edge connecting nodes 'a' and 'b' as '"(a,b)"'.
-- Allows for optional "branch length" annotation as '"(a,b):12.34"'.
edgeDefinition :: MonadParsec s m Char => m EdgeInfo
edgeDefinition = symbol $ do
    _ <- space
    _ <- symbol (char '(') 
    x <- symbol vertexLabelDefinition
    _ <- symbol (char ',') 
    y <- symbol vertexLabelDefinition
    _ <- symbol (char ')')
    z <- optional $ try branchLengthDefinition
    pure $ EdgeInfo (x,y) z
  where
    branchLengthDefinition = symbol (char ':') *> symbol double

-- | Convinence combinator to consume trailing whitespace.
symbol :: MonadParsec s m Char => m a -> m a
symbol x = x <* space

-- | Validates a parse result to ensure that the resulting forest is internally consistent.
-- A VER forest is not consistent if:
--
--   * Any two root nodes are connected
--
--   * Any tree contains a cycle
validateForest :: MonadParsec s m Char => VertexEdgeRoot -> m VertexEdgeRoot
validateForest ver@(VER vs es rs )
  | (not.null) treeEdgeCycles = fails $ edgeCycleErrorMessage <$> treeEdgeCycles
  | (not.null) connectedRoots = fails $ manyRootsErrorMessage <$> connectedRoots
  | otherwise                 = pure ver
  where
    rootList       = elems rs
    treeEdgeCycles = catMaybes       $ findCycle <$> rootList
    connectedRoots = filter manyList $ findRoots <$> rootList
    connections    = buildEdgeMap vs es
    manyList []  = False
    manyList [_] = False
    manyList _   = True
    -- Detect if the tree contains a cycle by consulting a stack
    -- while performing a depth-first-search
    findCycle :: VertexLabel -> Maybe (VertexLabel,[VertexLabel])
    findCycle root
      | null result = Nothing
      | otherwise   = Just (root,result)
      where
        result = findCycle' [] Nothing root
        findCycle' stack parent node
          | cycleDetected = cycle'
          | null children = []
          | otherwise     = maximumBy (comparing length) childCycles
          where
            (inner,base)  = span (/=node) stack
            cycleDetected = not $ null base
            cycle'        = [node] ++ inner ++ [node]
            childCycles   = findCycle' (node:stack) (Just node) <$> children
            adjacentNodes = fromMaybe [] $ node `lookup` connections
            children      = case parent of
                              Just x  -> x `delete` adjacentNodes
                              Nothing -> adjacentNodes

    -- Determine if multiple roots are connected by traversing the tree
    -- and checking each node for inclusion in the root set.
    findRoots :: VertexLabel -> [VertexLabel]
    findRoots root = findRoots' root root
      where
        findRoots' parent node = selfRoot ++ descendantRoots
          where
            selfRoot        = filter (`elem` rs) [node]
            descendantRoots = concat $ findRoots' node <$> children
            children        = parent `delete` adjacentNodes
            adjacentNodes   = fromMaybe [] $ node `lookup` connections

    edgeCycleErrorMessage (r,xs) = concat
      [ "In the tree rooted at '"
      , show r
      , "', the following cycle was detected: "
      , show xs
      ] 
    manyRootsErrorMessage xs = concat
      [ "Multiple root nodes detected in a single tree. "
      , "The following root nodes should form different trees, but thay are part of the same tree: "
      , show xs
      ]

-- | Convience method for building a connection 'Map' based on the existing edges in the graph.
buildEdgeMap :: Set VertexLabel -> Set EdgeInfo -> Map VertexLabel [VertexLabel]
buildEdgeMap vs es = foldr buildMap empty vs
  where
    edgeList       = edgeConnection <$> elems es
    buildMap  node = insert node (connected node)
    connected node = catMaybes $ f <$> edgeList
      where
        f (a,b)
          | a == node  = Just b
          | b == node  = Just a
          | otherwise  = Nothing

