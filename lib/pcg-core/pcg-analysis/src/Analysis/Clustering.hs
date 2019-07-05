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
  ( ClusterOptions
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

data ClusterOptions
  = Hierarchical H.Linkage
  | Median
  | None

pattern UPGMA :: ClusterOptions
pattern UPGMA = Hierarchical H.Average

pattern SingleLinkage :: ClusterOptions
pattern SingleLinkage = Hierarchical H.Single

pattern CompleteLinkage :: ClusterOptions
pattern CompleteLinkage = Hierarchical H.Complete

pattern UPGMALinkage :: ClusterOptions
pattern UPGMALinkage = Hierarchical H.Average

pattern WeightedLinkage :: ClusterOptions
pattern WeightedLinkage = Hierarchical H.Weighted

pattern WardLinkage :: ClusterOptions
pattern WardLinkage = Hierarchical H.Ward

pattern KMedians :: ClusterOptions
pattern KMedians = Median

pattern NoCluster :: ClusterOptions
pattern NoCluster = None


clusterIntoGroups
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> ClusterOptions
  -> Int
  -> NE.Vector (NE.Vector (DecoratedCharacterNode f))
clusterIntoGroups meta leaves clusterOption numberOfClusters =
  case clusterOption of
    Hierarchical hierarchicalOpt
      -> CH.clusterIntoGroups meta leaves hierarchicalOpt numberOfClusters

    Median -> error "Median clustering not yet implemented."
    None   -> error "ToDO"
