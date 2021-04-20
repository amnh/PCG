-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Clustering.Hierarchical
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

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


-- |
-- Determine how to split nodes into clustering groups.
data  ClusterCut
    = ClusterGroup Int
    | ClusterSplit Double


-- |
-- Determine how to measure distances between clusters.
data  ClusterOptions
    = Hierarchical H.Linkage ClusterCut
    | Median
    | None


-- |
-- View pattern for the unweighted pair group method with arithmetic mean (UPGMA)
-- clustering option.
pattern UPGMA :: ClusterCut -> ClusterOptions
pattern UPGMA s <- Hierarchical H.Average s
  where
    UPGMA s = Hierarchical H.Average s


-- |
-- View pattern for the single linkage clustering option.
pattern SingleLinkage :: ClusterCut ->  ClusterOptions
pattern SingleLinkage s <- Hierarchical H.Single s
  where
    SingleLinkage s = Hierarchical H.Single s


-- |
-- View pattern for the complete linkage clustering option.
pattern CompleteLinkage :: ClusterCut -> ClusterOptions
pattern CompleteLinkage s <- Hierarchical H.Complete s
  where
    CompleteLinkage s = Hierarchical H.Complete s


-- |
-- View pattern for the UPGMA-informed linkage clustering option.
pattern UPGMALinkage :: ClusterCut ->  ClusterOptions
pattern UPGMALinkage s <- Hierarchical H.Average s
  where
    UPGMALinkage s = Hierarchical H.Average s


-- |
-- View pattern for the linkage with weighting clustering option.
pattern WeightedLinkage :: ClusterCut -> ClusterOptions
pattern WeightedLinkage s <- Hierarchical H.Weighted s
  where
    WeightedLinkage s = Hierarchical H.Weighted s


-- |
-- View pattern for the linkage defined by Ward Wheeler clustering option.
pattern WardLinkage :: ClusterCut -> ClusterOptions
pattern WardLinkage s <- Hierarchical H.Ward s
  where
    WardLinkage s = Hierarchical H.Ward s


-- |
-- View pattern for the K-Medians linkage clustering option.
pattern KMedians :: ClusterOptions
pattern KMedians = Median


-- |
-- View pattern for the "no clustering," clustering option.
pattern NoCluster :: ClusterOptions
pattern NoCluster = None


-- |
-- Partition a leaf set into clusters based on the specified clustering options
-- and the metrics defined in the metadata sequence.
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
