{-# LANGUAGE PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Clustering.Hierarchical
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Analysis.Clustering
  ( ClusterOptions(..)
  , ClusterCut(..)
  , pattern UPGMA
  , pattern SingleLinkage
  , pattern CompleteLinkage
  , pattern UPGMALinkage
  , pattern WeightedLinkage
  , pattern WardLinkage
  , pattern KMedians
  , pattern NoCluster
  , clusterIntoGroups
  ) where

import qualified AI.Clustering.Hierarchical       as H
import qualified Analysis.Clustering.Hierarchical as CH
import           Bio.Graph.Constructions
import           Bio.Graph.LeafSet
import           Bio.Sequence
import qualified Data.Vector.NonEmpty             as NE


data ClusterCut
  = ClusterGroup Int
  | ClusterSplit Double

data ClusterOptions
  = Hierarchical H.Linkage ClusterCut
  | Median
  | None


pattern UPGMA :: ClusterCut -> ClusterOptions
pattern UPGMA s <- Hierarchical H.Average s
  where
    UPGMA s = Hierarchical H.Average s

pattern SingleLinkage :: ClusterCut ->  ClusterOptions
pattern SingleLinkage s <- Hierarchical H.Single s
  where
    SingleLinkage s = Hierarchical H.Single s

pattern CompleteLinkage :: ClusterCut -> ClusterOptions
pattern CompleteLinkage s <- Hierarchical H.Complete s
  where
    CompleteLinkage s = Hierarchical H.Complete s

pattern UPGMALinkage :: ClusterCut ->  ClusterOptions
pattern UPGMALinkage s <- Hierarchical H.Average s
  where
    UPGMALinkage s = Hierarchical H.Average s

pattern WeightedLinkage :: ClusterCut -> ClusterOptions
pattern WeightedLinkage s <- Hierarchical H.Weighted s
  where
    WeightedLinkage s = Hierarchical H.Weighted s

pattern WardLinkage :: ClusterCut -> ClusterOptions
pattern WardLinkage s <- Hierarchical H.Ward s
  where
    WardLinkage s = Hierarchical H.Ward s

pattern KMedians :: ClusterOptions
pattern KMedians = Median

pattern NoCluster :: ClusterOptions
pattern NoCluster = None


clusterIntoGroups
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> ClusterOptions
  -> NE.Vector (NE.Vector (DecoratedCharacterNode f))
clusterIntoGroups meta leaves clusterOption =
  case clusterOption of
    Hierarchical linkage cut -> case cut of
      ClusterGroup n -> CH.clusterIntoGroups meta leaves linkage n
      ClusterSplit d -> CH.clusterIntoCuts   meta leaves linkage d
    Median -> error "Median clustering not yet implemented."
    None   -> error "ToDO"
