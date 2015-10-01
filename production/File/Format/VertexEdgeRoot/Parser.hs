{-# LANGUAGE FlexibleContexts #-}

module File.Format.VertexEdgeRoot.Parser 
  ( VertexEdgeRoot
  , edges
  , parseVertexEdgeRootStream
  , roots
  , verticies 
  ) where

import Data.Either           (partitionEithers)
import Data.List             (partition,sortBy)
import Data.Ord              (comparing)
import Data.Set              (Set,fromList,size)
import Text.Parsec
import Text.Parsec.Custom

type VertexLabel   = String
type EdgeLength    = Maybe Double

data VertexSetType = Verticies | Roots deriving (Eq)
data EdgeInfo      = EdgeInfo (VertexLabel,VertexLabel) EdgeLength deriving (Show,Eq,Ord)
data VertexEdgeRoot
   = VertexEdgeRoot
   { verticies   :: Set VertexLabel
   , edges       :: Set EdgeInfo
   , roots       :: Set VertexLabel
   } deriving (Show)

parseVertexEdgeRootStream :: Stream s m Char => s -> m (Either ParseError VertexEdgeRoot)
parseVertexEdgeRootStream = runParserT (verDefinition <* eof) () "A set of verticies, a set of edges, and a set of root verticies"

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
verDefinition :: Stream s m Char => ParsecT s u m VertexEdgeRoot
verDefinition = do
    sets <- many setDefinition
    case partitionEithers sets of
      ([edges'], [x,y]) -> formVertexEdgeRoot x y edges'
      (xs      , ys   ) -> runFail $ edgeSetMessages xs ++ vertexSetMessages ys
  where
    formVertexEdgeRoot x@(typeA, setA) y@(typeB, setB) edges' =
      case (typeA, typeB) of
        (Nothing       , Nothing       ) -> let [m,n] = sortBy (comparing size) [setA,setB]
                                            in pure $ VertexEdgeRoot n    edges' m
        (Nothing       , Just Verticies) ->    pure $ VertexEdgeRoot setB edges' setA
        (Nothing       , Just Roots    ) ->    pure $ VertexEdgeRoot setA edges' setB
        (Just Verticies, Nothing       ) ->    pure $ VertexEdgeRoot setA edges' setB
        (Just Roots    , Nothing       ) ->    pure $ VertexEdgeRoot setB edges' setA
        (Just Verticies, Just Roots    ) ->    pure $ VertexEdgeRoot setA edges' setB 
        (Just Roots    , Just Verticies) ->    pure $ VertexEdgeRoot setB edges' setA
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

-- We read a set from the input. The set can be an edge set or a vertex set.
-- If it is a vertex set, it may be labeled as a specific set of verticies or
-- a set of roots. We use the Either type as a return type to denote the 
-- conditional type of the result.
setDefinition :: Stream s m Char => ParsecT s u m (Either (Set EdgeInfo) (Maybe VertexSetType, Set VertexLabel))
setDefinition = do
    result <- optionMaybe (try edgeSetDefinition)
    case result of
      Just x  -> pure $ Left x
      Nothing -> Right <$> vertexSetDefinition

-- A vertex set can be labeled or unlabeled. We first attempt to read in a 
-- labeled vertex set, and if that fails an unlabeled vertex set. The label
-- is returned contidionally in a Maybe type.
vertexSetDefinition :: Stream s m Char => ParsecT s u m (Maybe VertexSetType, Set VertexLabel)
vertexSetDefinition = try labeledVertexSetDefinition <|> unlabeledVertexSetDefinition

-- A labeled vertex set contains a label followed by an unlabeled vertex set
labeledVertexSetDefinition :: Stream s m Char => ParsecT s u m (Maybe VertexSetType, Set VertexLabel)
labeledVertexSetDefinition = do
    setType <- vertextSetType
    _       <- symbol (char '=')
    (_,set) <- unlabeledVertexSetDefinition
    pure (Just setType, set)

-- A vertex set label is one of the following case insensative strings:
-- "vertexset", rootset"
vertextSetType :: Stream s m Char => ParsecT s u m VertexSetType
vertextSetType = do
    value <- optionMaybe (try (symbol (caseInsensitiveString "VertexSet")))
    case value of
      Just _  -> pure Verticies
      Nothing -> symbol (caseInsensitiveString "RootSet") *> pure Roots

unlabeledVertexSetDefinition :: Stream s m Char => ParsecT s u m (Maybe VertexSetType, Set VertexLabel)
unlabeledVertexSetDefinition = do
    _       <- symbol (char '{')
    labels' <- vertexLabelDefinition `sepBy` symbol (char ',')
    _       <- symbol (char '}')
    pure (Nothing, fromList labels')
    
vertexLabelDefinition :: Stream s m Char => ParsecT s u m String
vertexLabelDefinition = many1 alphaNum

edgeSetDefinition :: Stream s m Char => ParsecT s u m (Set EdgeInfo)
edgeSetDefinition = do
    _      <- optional (try edgeSetLabel)
    _      <- symbol (char '{')
    pairs  <- edgePairingDefinition `sepBy` symbol (char ',')
    _      <- symbol (char '}')
    pure $ fromList pairs
  where
    edgePairingDefinition = do 
      _ <- symbol (char '(') 
      x <- symbol vertexLabelDefinition
      _ <- symbol (char ',') 
      y <- symbol vertexLabelDefinition
      _ <- symbol (char ')')
      z <- optionMaybe (try branchLengthDefinition)
      pure $ EdgeInfo (x,y) z
    branchLengthDefinition = symbol (char ':') *> symbol decimal
    edgeSetLabel = symbol (caseInsensitiveString "EdgeSet") <* symbol (char '=')

symbol :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
symbol x = x <* spaces
    