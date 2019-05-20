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

import Analysis.Clustering.Hierarchical as H
import           AI.Clustering.Hierarchical

data ClusterOptions = ClusterOpt Linkage

pattern UPGMA :: ClusterOptions
pattern UPGMA = ClusterOpt Average

cluster :: Int
cluster = undefined
