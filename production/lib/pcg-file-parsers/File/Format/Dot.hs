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
  , dotChildMap
  , dotParentMap
  , dotStreamParser
  , dotNodeSet
  , dotEdgeSet
  , toIdentifier
  ) where


import           Control.Arrow             ((&&&))
import           Data.GraphViz.Parsing
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Generalised
import           Data.Key
import           Data.Map                  (Map, fromSet, insert, insertWith, mergeWithKey, notMember)
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set           as S
import           Data.Text.Lazy            (Text)
import           Prelude            hiding (lookup)


dotNodeSet :: Ord n => DotGraph n -> Set n
dotNodeSet = foldMap (S.singleton . nodeID) . graphNodes


dotEdgeSet :: Ord n => DotGraph n -> Set (n, n)
dotEdgeSet = foldMap (S.singleton . (fromNode &&& toNode)) . graphEdges


dotStreamParser :: Text -> Either String (DotGraph GraphID)
dotStreamParser = fst . runParser parse
        

dotChildMap :: Ord n => DotGraph n -> Map n (Set n)
dotChildMap dot = fromSet getAdjacency nodes
  where
    getAdjacency = fromMaybe mempty . (`lookup` edges)
    nodes = dotNodeSet dot 
    edges = edgeMap dot

    -- Get the map of directed edges.
    -- Missing nodes with out degree 0.
    edgeMap :: Ord n => DotGraph n -> Map n (Set n)
    edgeMap = foldr mapFold mempty . dotEdgeSet
      where
        mapFold (k,v) m = insertWith (<>) k (S.singleton v) m

  
dotParentMap :: Ord n => DotGraph n -> Map n (Set n)
dotParentMap dot = fromSet getAdjacency nodes
  where
    getAdjacency = fromMaybe mempty . (`lookup` edges)
    nodes = dotNodeSet dot 
    edges = edgeMap dot

    -- Get the map of directed edges.
    -- Missing nodes with out degree 0.
    edgeMap :: Ord n => DotGraph n -> Map n (Set n)
    edgeMap = foldr mapFold mempty . dotEdgeSet
      where
        mapFold (k,v) m = insertWith (<>) v (S.singleton k) m

  
toIdentifier :: GraphID -> String
toIdentifier (Str x) = show x
toIdentifier (Num x) = show x

