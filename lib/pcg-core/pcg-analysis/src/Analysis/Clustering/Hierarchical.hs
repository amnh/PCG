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

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Clustering.Hierarchical where

import           AI.Clustering.Hierarchical
import           Analysis.Clustering.Metric
import           Bio.Graph.Constructions
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG
import           Bio.Sequence
import           Control.Lens
import           Control.Monad.ST
import           Data.Coerce
import           Data.DList                 (DList)
import qualified Data.DList                 as DL (toList)
import           Data.Monoid                (Sum (..))
import           Data.Vector
import           Data.Vector.Mutable        (STVector)
import qualified Data.Vector.Mutable        as MV


clusterLeaves
  :: CharacterDAG
  -> Linkage
  -> LeafSet CharacterNode
clusterLeaves dag opt =
  let
    leaves = (dag ^. leafSet)
    meta   = (dag ^. _columnMetadata)
  in
    clusterShuffle meta leaves opt

clusterShuffle
  :: MetadataSequence m
  -> LeafSet CharacterNode
  -> Linkage
  -> LeafSet CharacterNode
clusterShuffle meta leaves opt = clusteredLeafSet

  where
    leafSetVector :: Vector CharacterNode
    leafSetVector = fromLeafSet leaves

    distance :: CharacterNode -> CharacterNode -> Double
    distance node1 node2 =
      let
        charSeq1 = node1 ^. _sequenceDecoration
        charSeq2 = node2 ^. _sequenceDecoration
      in
        coerce $ characterSequenceDistance meta charSeq1 charSeq2


    dendro :: Dendrogram CharacterNode
    dendro = hclust opt leafSetVector distance

    clusteredLeafSet :: LeafSet CharacterNode
    clusteredLeafSet = coerce $ dendroToVector dendro



dendroToList :: Dendrogram a -> DList a
dendroToList = \case
  Leaf a -> pure a
  Branch _ _ l r -> dendroToList l <> dendroToList r


dendroToVector
  :: Dendrogram a
  -> Vector a
dendroToVector dendro = create $ dendroToMVector dendro

dendroToVectorClusters
  :: Dendrogram a
  -> Int
  -> Vector (Vector a)
dendroToVectorClusters d n = case d of
    Leaf a -> pure $ pure a
    b@(Branch tot _ left right) ->
      case n of
        1 -> pure $ dendroToVector b
        k ->
          let
            leftAmount = (floor (fromIntegral tot / fromIntegral (size left))) * k
            rightAmount = k - leftAmount
          in
            (dendroToVectorClusters left leftAmount) <> (dendroToVectorClusters right rightAmount)

dendroToMVector
  :: forall a s
  .  Dendrogram a
  -> ST s (STVector s a)
dendroToMVector =
  \case
    Leaf a ->
      do
        m <- MV.new 1
        MV.write m 0 a
        pure m

    Branch tot _ left right ->
        do
          let sl = size left
          leftM  <- dendroToMVector left
          rightM <- dendroToMVector right
          unsafeAppendMVector tot sl leftM rightM


appendMVector :: STVector s a -> STVector s a -> ST s (STVector s a)
appendMVector v1 v2 =
    do
      let l1 = MV.length v1
          l2 = MV.length v2
      result <- MV.new (l1 + l2)
      MV.copy (MV.take l1 result) v1
      MV.copy (MV.drop l1 result) v2
      pure result


unsafeAppendMVector
  :: Int  -- ^ total size
  -> Int  -- ^ size of left vector
  -> STVector s a
  -> STVector s a
  -> ST s (STVector s a)
unsafeAppendMVector tot sl v1 v2 =
  do
    result <- MV.new tot
    MV.copy (MV.take sl result) v1
    MV.copy (MV.drop sl result) v2
    pure result



