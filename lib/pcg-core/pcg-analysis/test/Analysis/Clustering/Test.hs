-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynmaic.DirectOptimization.Pairwise.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for general analysis operations
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Clustering.Test
  ( testSuite
  ) where


import Analysis.Clustering.Hierarchical
import qualified Data.Vector.NonEmpty as NE
import Data.Vector hiding (length)
import Data.Foldable
import           AI.Clustering.Hierarchical
import           Test.Tasty
import           Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "Clustering Tests"
    [ hierarchicalClusteringProperties
    ]


hierarchicalClusteringProperties :: TestTree
hierarchicalClusteringProperties = testGroup "Properties of hierarchical clustering"
    [ testProperty "Clustering preserves the number of inputs" preservesNumber
    ]

preservesNumber :: NonEmptyList Double -> Property
preservesNumber lv =
    length v === length (afterCluster)
  where
    v :: Vector Double
    v = fromList . getNonEmpty $ lv


    afterCluster :: Vector Double
    afterCluster = fold . NE.toVector . fmap (NE.toVector) $ clusters

    clusters :: NE.Vector (NE.Vector Double)
    clusters = dendroToVectorClusters dendro 20

    dendro :: Dendrogram Double
    dendro = hclust Average v (\x y -> sqrt (x^2 + y^2))
