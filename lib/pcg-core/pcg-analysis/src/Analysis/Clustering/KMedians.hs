-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Clustering.KMedians
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}



module Analysis.Clustering.KMedians where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid (Sum)
import VectorBuilder.Builder (Builder)
import qualified VectorBuilder.Builder as VB
import qualified Data.Vector.Mutable as MVector
import Data.Foldable
import Control.Monad.ST (ST)
import Data.Ord (comparing)
import VectorBuilder.Vector (build)

class HasDistance a where
  dist :: a -> a -> Sum Double

data MediansCluster a = MediansCluster
  { clusterPoints :: Vector (Int, a)
  , assignment    :: Vector (Builder a)
  }

data MedianOpts = MedianOpts
  { numberOfClusters   :: !Int
  , numberOfIterations :: !Int
  }

kMediansCluster
  :: (HasDistance a, Ord a)
  => (Vector a -> a)
  -> Vector a
  -> MedianOpts
  -> Vector (Vector a)
kMediansCluster kMedians inputs opts =
  let
     initial  = initialAssignment kMedians inputs opts
     final    = lloydMedians kMedians inputs opts initial
  in
    build <$> (assignment final)


initialAssignment
  :: forall a 
  . (Vector a -> a)
  -> Vector a
  -> MedianOpts
  -> MediansCluster a
initialAssignment kMedians inputs MedianOpts{..} =
  let
    inputSize = length inputs
    chunkSize = ceiling $ fromIntegral @_ @Double inputSize / fromIntegral @_ @Double numberOfClusters
    chunks = chunksOf chunkSize inputs
    clusterPoints = kMedians <$> chunks
  in
    MediansCluster
    { clusterPoints = V.indexed clusterPoints
    , assignment    = VB.vector <$> chunks
    }


lloydMedians
  :: forall a . (HasDistance a, Eq a)
  => (Vector a -> a)
  -> Vector a
  -> MedianOpts
  -> MediansCluster a
  -> MediansCluster a
lloydMedians kMedian inputs MedianOpts{..} medianClusters = go 0 medianClusters
  where
    go :: Int -> MediansCluster a -> MediansCluster a
    go iter currClusters = 
      let
        currAssignmentV :: Vector (Vector a)
        currAssignmentV = build <$> (assignment currClusters)
        newClusterPoints = V.indexed (kMedian <$> currAssignmentV)
        newCluster = assignClusters newClusterPoints inputs
        newAssignmentV :: Vector (Vector a)
        newAssignmentV = build <$> (assignment newCluster)
      in
        if (iter >= numberOfIterations) then
        currClusters
        else
           if currAssignmentV == newAssignmentV then
             newCluster
           else
             go (iter + 1) newCluster


assignClusters
  :: forall a . (HasDistance a)
  => Vector (Int, a)
  -> Vector a
  -> MediansCluster a
assignClusters clusterPoints inputs = MediansCluster{..}
  where
    assignment :: Vector (Builder a)
    assignment  = V.create $ do
      let numberOfClusters = length clusterPoints
      vec <- MVector.replicate numberOfClusters VB.empty
      let
    --    addPoint :: a -> ST s ()
        addPoint a = do
           let clusterLabel = nearest a
           currCluster <- MVector.read vec clusterLabel
           MVector.write vec clusterLabel $ currCluster <> VB.singleton a

        nearest :: a -> Int
        nearest a = fst $ minimumBy (comparing (f a)) clusterPoints

        f :: a -> ((Int, a) -> Sum Double)
        f inp (_, clusterPoint) = dist inp clusterPoint
      traverse_ addPoint inputs
      pure vec


chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf chunk vec = 
  let
    len = length vec
  in
    build $ go len chunk vec
  where
    go :: Int -> Int -> Vector a -> Builder (Vector a)
    go l c v =
      if l >= c then
        VB.singleton (V.take c v) <> (go (l - c) c (V.drop c v))
      else
        VB.singleton v
                
