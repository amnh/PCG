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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

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
  ) where


import           Control.Arrow             ((&&&))
import           Data.GraphViz.Parsing
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Generalised
import           Data.Key
import           Data.Map                  (Map, fromSet, insertWith)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set           as S
import           Data.Text                 (Text)
import qualified Data.Text.Lazy     as L
import           Prelude            hiding (lookup)


dotParse :: Text -> Either String (DotGraph GraphID)
dotParse = fst . runParser parse . L.fromStrict
        

dotNodeSet :: Ord n => DotGraph n -> Set n
dotNodeSet = foldMap (S.singleton . nodeID) . graphNodes


dotEdgeSet :: Ord n => DotGraph n -> Set (n, n)
dotEdgeSet = foldMap (S.singleton . (fromNode &&& toNode)) . graphEdges


dotChildMap :: Ord n => DotGraph n -> Map n (Set n)
dotChildMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) k (S.singleton v)

  
dotParentMap :: Ord n => DotGraph n -> Map n (Set n)
dotParentMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) v (S.singleton k)
    

sharedWork :: Ord n => ((n, n) -> Map n (Set n) -> Map n (Set n)) -> DotGraph n -> Map n (Set n)
sharedWork logic dot = fromSet getAdjacency setOfNodes
  where
    -- Get the map of directed edges.
    -- Missing nodes with out degree 0.
    edgeMap      = foldr logic mempty . dotEdgeSet
    getAdjacency = fromMaybe mempty . (`lookup` setOfEdges)
    setOfEdges   = edgeMap    dot
    setOfNodes   = dotNodeSet dot 

  
toIdentifier :: GraphID -> String
toIdentifier (Str x) = L.unpack x
toIdentifier (Num x) = show x

