-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Dot
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module File.Format.Dot
  ( DotGraph
  , GraphID(..)
  -- ** Parse a DOT file
  , dotParse
  -- ** Get useful representations from the DOT file
  , dotChildMap
  , dotParentMap
  , dotNodeSet
  , dotEdgeSet
  , toIdentifier
  , NodeLabel()
  , nodeLabel
  , strLabel
  ) where


import           Control.Arrow                   ((&&&))
import           Data.Foldable
import           Data.GraphViz.Parsing
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Generalised
import           Data.Key
import           Data.Map                        (Map, fromSet, insertWith)
import           Data.Monoid
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as L
import           Prelude                         hiding (lookup)
import Data.Monoid (First(getFirst))
import Data.GraphViz.Attributes.Complete (Attribute(Label), Label(..))


-- |
-- Parses the 'Text' stream from a DOT file.
dotParse :: Text -> Either String (DotGraph GraphID)
dotParse = fst . runParser parse . L.fromStrict

newtype NodeLabel n = NodeLabel {getLabel :: (Either L.Text n)}
  deriving stock (Eq, Show, Ord)

nodeLabel :: Ord n => n -> NodeLabel n
nodeLabel = NodeLabel . Right

strLabel :: L.Text -> NodeLabel n
strLabel = NodeLabel . Left

type EdgeIdentifier n = (n , n)

-- |
-- Takes a 'DotGraph' parse result and returns a set of unique node identifiers.
dotNodeSet :: Ord n => DotGraph n -> Set (NodeLabel n)
dotNodeSet = foldMap (S.singleton . nodeName) . graphNodes
  where
    nodeName :: Ord n' => DotNode n' -> NodeLabel n'
    nodeName n =
          case getFirst (foldMap getStrLabel (nodeAttributes n)) of
            Nothing  -> nodeLabel . nodeID $ n
            Just str -> strLabel $ str

    getStrLabel :: Attribute -> First L.Text
    getStrLabel (Label (StrLabel txt)) = First . Just $ txt
    getStrLabel _                      = mempty

dotNodeIdentifierSet :: Ord n => DotGraph n -> Set n
dotNodeIdentifierSet = foldMap (S.singleton . nodeID) . graphNodes

-- |
-- Takes a 'DotGraph' parse result and returns a set of unique edge identifiers.
dotEdgeSet :: Ord n => DotGraph n -> Set (EdgeIdentifier n)
dotEdgeSet = foldMap (S.singleton . (fromNode &&& toNode)) . graphEdges


-- |
-- Takes a 'DotGraph' parse result and constructs a mapping from a node to it's
-- children.
dotChildMap :: Ord n => DotGraph n -> Map (NodeLabel n) (Set n)
dotChildMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) (nodeLabel k) (S.singleton v)


-- |
-- Takes a 'DotGraph' parse result and constructs a mapping from a node to it's
-- parents.
dotParentMap :: Ord n => DotGraph n -> Map (NodeLabel n) (Set n)
dotParentMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) (nodeLabel v) (S.singleton k)

-- |
-- Intelligently render a 'NodeLabel' of a 'GraphID' to a 'String' for output.
toIdentifier :: NodeLabel GraphID -> String
toIdentifier = either L.unpack graphIDIdentifier . getLabel
  where
    graphIDIdentifier :: GraphID -> String
    graphIDIdentifier (Str x) = L.unpack x
    graphIDIdentifier (Num x) = show x


-- |
-- The shared work between generating the 'dotChildMap' and 'dotParentMap'
-- functions.
sharedWork
  :: forall n. Ord n
  => (  EdgeIdentifier n
     -> Map (NodeLabel n) (Set n)
     -> Map (NodeLabel n) (Set n)
     )
  -> DotGraph n
  -> Map (NodeLabel n) (Set n)
sharedWork logic dot = fromSet getAdjacency setOfNodes
  where
    -- Get the map of directed edges.
    -- Missing nodes with out degree 0.
--    edgeMap :: DotGraph n -> Map (Node (Set n)
    edgeMap      = foldr logic mempty . dotEdgeSet
--    getAdjacency :: n -> Set n
    getAdjacency = fold . (`lookup` setOfEdges)
--    setOfEdges :: Map n (Set n)
    setOfEdges   = edgeMap    dot
--     setOfNodes = Set (NodeIdentiifer n)
    setOfNodes   = dotNodeSet dot
