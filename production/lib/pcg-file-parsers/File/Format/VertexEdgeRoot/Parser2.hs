{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.VertexEdgeRoot.Parser2 where

import Data.Char              (isSpace)
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
import Text.Megaparsec.Perm
import Text.Megaparsec.Prim   (MonadParsec)

type VertexLabel   = String
type EdgeLength    = Maybe Double

data VertexSetType = Verticies | Edges | Roots deriving (Eq,Show)
data EdgeInfo      = EdgeInfo (VertexLabel,VertexLabel) EdgeLength deriving (Show,Eq,Ord)
data VertexEdgeRoot
   = VER
   { verticies   :: Set VertexLabel
   , edges       :: Set EdgeInfo
   , roots       :: Set VertexLabel
   } deriving (Show)


data NodeSet
   = Labeled   (Set VertexLabel) String
   | Unlabeled (Set VertexLabel)
type VertexSet = NodeSet
type EdgeSet   = Set EdgeInfo
type RootSet   = NodeSet

getNodes :: NodeSet -> Set VertexLabel
getNodes (Labeled s _) = s
getNodes (Unlabeled s) = s

edgeConnection :: EdgeInfo -> (VertexLabel,VertexLabel)
edgeConnection (EdgeInfo (a,b) _)
  | a <= b    = (a,b)
  | otherwise = (b,a)

-- | Reads two vertex sets and an edge set, conditionally infers the root set
-- when vertex sets are unlabeled. Ensures that the elements of the root set
-- are not connected in the forest. Ensures that the rooted trees in the
-- forest do not contain cycles.
verStreamParser :: (MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
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
verDefinition :: (MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
verDefinition = perm

{-
verDefinition :: (MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
verDefinition = do
    sets <- many setDefinition
    case partitionEithers sets of
      ([edges'], [x,y]) -> formVertexEdgeRoot x y edges'
      (xs      , ys   ) -> runFail $ edgeSetMessages xs ++ vertexSetMessages ys
  where
    formVertexEdgeRoot x@(typeA, setA) y@(typeB, setB) edges' =
      case (typeA, typeB) of
        (Nothing       , Nothing       ) -> let [m,n] = sortBy (comparing size) [setA,setB]
                                            in pure $ VER n    edges' m
        (Nothing       , Just Verticies) ->    pure $ VER setB edges' setA
        (Nothing       , Just Roots    ) ->    pure $ VER setA edges' setB
        (Just Verticies, Nothing       ) ->    pure $ VER setA edges' setB
        (Just Roots    , Nothing       ) ->    pure $ VER setB edges' setA
        (Just Verticies, Just Roots    ) ->    pure $ VER setA edges' setB 
        (Just Roots    , Just Verticies) ->    pure $ VER setB edges' setA
        (_             , _             ) -> runFail $ vertexSetMessages [x,y]
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
-}

perm :: (MonadParsec e s m, Token s ~ Char) => m VertexEdgeRoot
perm = do
  (v,e,r) <- makePermParser $ (,,) <$$> vertexSet <||> edgeSet <||> rootSet
  let v' = getNodes v
  let r' = getNodes r
  pure $ case (v,r) of
           (Unlabeled xs, Unlabeled ys) -> if length xs >= length ys
                                           then VER v' e r'
                                           else VER r' e v'
           _                            -> VER v' e r'

labeledNodeSet :: (MonadParsec e s m, Token s ~ Char) => String -> m NodeSet
labeledNodeSet setLabel = do
    l  <- vertexSetLabel <?> "set label '" ++ setLabel ++ "'"
    ns <- unlabeledNodeSet
    pure $ case ns of
             Unlabeled xs -> Labeled xs l
             _            -> ns -- Should never happen
  where
    vertexSetLabel = do
        l <- symbol $ string' setLabel
        _ <- symbol $ char '='
        pure l 
    
                   
-- A labeled vertex set contains a label followed by an unlabeled vertex set
rootSet, vertexSet :: (MonadParsec e s m, Token s ~ Char) => m NodeSet
rootSet   = (labeledNodeSet "RootSet"   <|> unlabeledNodeSet) <?> "root set definition"
vertexSet = (labeledNodeSet "VertexSet" <|> unlabeledNodeSet) <?> "vertex set definition"

-- A vertex set with an optional set label enclosed in braces.
-- A vertex set cannot have duplicate verticies
unlabeledNodeSet :: (MonadParsec e s m, Token s ~ Char) => m NodeSet
unlabeledNodeSet = validateNodeSet =<< unlabeledNodeSet'
  where
    unlabeledNodeSet' :: (MonadParsec e s m, Token s ~ Char) => m [VertexLabel]
    unlabeledNodeSet' = do
        _       <- symbol (char '{')
        labels' <- vertexLabel `sepBy1` symbol (char ',')
        _       <- symbol (char '}')
        pure labels'
    validateNodeSet :: (MonadParsec e s m, Token s ~ Char) => [VertexLabel] -> m NodeSet
    validateNodeSet ns
      | null dupes = pure . Unlabeled $ fromList ns
      | otherwise  = fail errorMessage
      where
        dupes = duplicates ns
        errorMessage = "The following verticies were defined multiple times: " ++ show dupes

-- A vertex label is any non-space character that is also not a brace, paren, or comma.
vertexLabel :: (MonadParsec e s m, Token s ~ Char) => m String
vertexLabel = some validChar
  where
    validChar = satisfy $ \x -> x `notElem` "{}(),"
                             && (not . isSpace) x

-- Parses an edge set with an optional edge set label.
-- Edges cannot be from one node to the same node.
-- Edges are undirected, with duplicate edges prohibited.
edgeSet :: (MonadParsec e s m, Token s ~ Char) => m (Set EdgeInfo)
edgeSet = validateEdgeSet =<< (edgeSet' <?> "edge set definition")
  where
    edgeSetLabel :: (MonadParsec e s m, Token s ~ Char) => m String
    edgeSetLabel = (symbol (string' "EdgeSet") <* symbol (char '=')) <?> "edge set label"

    edgeSet' :: (MonadParsec e s m, Token s ~ Char) => m [EdgeInfo]
    edgeSet' = do
        _      <- optional (try edgeSetLabel)
        _      <- symbol (char '{')
        pairs  <- edgeDefinition `sepBy` symbol (char ',')
        _      <- symbol (char '}')
        pure pairs

    validateEdgeSet :: (MonadParsec e s m, Token s ~ Char) => [EdgeInfo] -> m (Set EdgeInfo)
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

edgeDefinition :: (MonadParsec e s m, Token s ~ Char) => m EdgeInfo
edgeDefinition = symbol $ do
    _ <- space
    _ <- symbol (char '(') 
    x <- symbol vertexLabel
    _ <- symbol (char ',') 
    y <- symbol vertexLabel
    _ <- symbol (char ')')
    z <- optional $ try branchLengthDefinition
    pure $ EdgeInfo (x,y) z
  where
    branchLengthDefinition = symbol (char ':') *> symbol double

symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol x = x <* space

-- A VER forest is not valid if:
--   * Any two root nodes are connected
--   * Any tree contains a cycle
validateForest :: (MonadParsec e s m, Token s ~ Char) => VertexEdgeRoot -> m VertexEdgeRoot
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

