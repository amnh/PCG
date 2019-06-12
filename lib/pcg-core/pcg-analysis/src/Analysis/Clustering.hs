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
  , clusterIntoGroups
  ) where

import qualified AI.Clustering.Hierarchical       as H
import qualified Analysis.Clustering.Hierarchical as CH
import           Bio.Graph.Constructions
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Sequence
import qualified Data.Vector.NonEmpty             as NE

data ClusterOptions
  = Hierarchical H.Linkage
  | Median

pattern UPGMA :: ClusterOptions
pattern UPGMA = Hierarchical H.Average


clusterIntoGroups
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> ClusterOptions
  -> Int
  -> NE.Vector (NE.Vector (DecoratedCharacterNode f))
clusterIntoGroups meta leafSet clusterOption numberOfClusters =
  case clusterOption of
    Hierarchical hierarchicalOpt
      -> CH.clusterIntoGroups meta leafSet hierarchicalOpt numberOfClusters

    Median -> error "Median clustering not yet implemented."
