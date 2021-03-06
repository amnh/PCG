-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
--  , example
  ) where


import           AI.Clustering.Hierarchical
import           AI.Clustering.Hierarchical.Types (Distance)
import           Analysis.Clustering.Hierarchical
import           Data.Foldable
import           Data.Vector                      hiding (length)
import qualified Data.Vector.NonEmpty             as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck


-- |
-- The test-suite for the clustering build algorithm.
testSuite :: TestTree
testSuite = testGroup "Clustering Tests"
    [ hierarchicalClusteringProperties
    ]


{-
example :: IO ()
example =
  do
    putStrLn . drawDendrogram $ fmap show myDendro
    putStrLn ""
    print cut
  where
    cut = myDendro `cutCluster` 0.7

    inputs = fromList  [1,1,1,1,4,10,11,804,814,834, 769]

    myDendro :: Dendrogram Double
    myDendro = hclust Average inputs dist
-}


hierarchicalClusteringProperties :: TestTree
hierarchicalClusteringProperties = testGroup "Properties of hierarchical clustering"
    [ testProperty "Clustering preserves the number of inputs" preservesNumber
    ]


preservesNumber :: NonEmptyList Double -> Property
preservesNumber lv =
    length v === length afterCluster
  where
    v :: Vector Double
    v = fromList . getNonEmpty $ lv


    afterCluster :: Vector Double
    afterCluster = fold . NE.toVector . fmap NE.toVector $ clusters

    clusters :: NE.Vector (NE.Vector Double)
    clusters = dendroToVectorClusters dendro 20

    dendro :: Dendrogram Double
    dendro = hclust Average v dist


dist :: Distance -> Distance -> Distance
dist x y = sqrt (x^two + y^two)
  where
    two = 2 :: Word
