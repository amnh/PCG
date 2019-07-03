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
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Analysis.Clustering.Hierarchical where

import           AI.Clustering.Hierarchical
import           Analysis.Clustering.Metric
import           Bio.Graph.Constructions
import           Bio.Graph.LeafSet
import Bio.Graph.Node (HasSequenceDecoration(..))
import           Bio.Sequence
import           Control.Lens
import           Control.Monad.ST
import           Data.Coerce
import           Data.DList                 (DList)
import           Data.Monoid                (Sum (..))
import           Data.Vector hiding (length, toList)
import qualified Data.Vector.NonEmpty       as NE
import VectorBuilder.Builder (Builder)
import qualified VectorBuilder.Builder as VB
import VectorBuilder.Vector (build)
import Prelude hiding (length)
import Data.Foldable
import Data.Clustering.Hierarchical as H
import Debug.Trace

clusterLeaves
  :: forall f m . (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> Linkage
  -> Dendrogram (DecoratedCharacterNode f)
{-# INLINE clusterLeaves #-}
clusterLeaves meta leaves opt = dendro
  where
    leafSetVector :: Vector (DecoratedCharacterNode f)
    leafSetVector = force $ fromLeafSet leaves

    distance :: DecoratedCharacterNode f -> DecoratedCharacterNode f -> Double
    distance node1 node2 =
      let
        charSeq1 = node1 ^. _sequenceDecoration
        charSeq2 = node2 ^. _sequenceDecoration
      in
        getSum $ characterSequenceDistance @f meta charSeq1 charSeq2

    dendro :: Dendrogram (DecoratedCharacterNode f)
    dendro =
      hclust opt (toList leafSetVector) distance


clusterShuffle
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> Linkage
  -> LeafSet (DecoratedCharacterNode f)
clusterShuffle meta leaves = coerce . dendroToVector . clusterLeaves meta leaves


clusterIntoGroups
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> Linkage
  -> Int
  -> NE.Vector (NE.Vector (DecoratedCharacterNode f))
clusterIntoGroups meta leaves link n =
    dendroToVectorClusters dendro n
  where
    dendro = clusterLeaves meta leaves link


dendroToList :: Dendrogram a -> DList a
dendroToList = \case
  Leaf a -> pure a
  Branch _ _ l r -> dendroToList l <> dendroToList r


dendroToVector
  :: Dendrogram a
  -> Vector a
dendroToVector = foldMap pure
--    build . vectorBuilder
--  where
--    vectorBuilder :: Dendrogram a -> Builder a
--    vectorBuilder = \case
--      Leaf a -> VB.singleton a
--      Branch _ _ d1 d2 -> vectorBuilder d1 <> vectorBuilder d2
  -- create $ dendroToMVector dendro


dendroToNonEmptyVector
  :: Dendrogram a
  -> NE.Vector a
dendroToNonEmptyVector =
    NE.unsafeFromVector -- This is safe to use as the clustering always uses non-empty input.
  . dendroToVector


dendroToVectorClusters
  :: Dendrogram a
  -> Int
  -> NE.Vector (NE.Vector a)
{-# INLINE dendroToVectorClusters #-}
dendroToVectorClusters _ 0 = error "Cannot return zero clusters"
dendroToVectorClusters d n = case d of
    Leaf a -> pure $ pure a
    b@(Branch totalNumber _ leftTree rightTree) ->
      case n of
        1 -> pure $ dendroToNonEmptyVector b
        k ->
          let
            leftSize, rightSize, totalSize, k' :: Double
            leftSize = fromIntegral $ size leftTree
            totalSize = fromIntegral totalNumber
            rightSize = fromIntegral $ size rightTree
            k' = fromIntegral k
            
            reOrder = if rightSize > leftSize then (\(a,b) -> (b,a)) else id
            (largerTree, smallerTree) = reOrder (leftTree, rightTree)
            (largerSize, smallerSize) = reOrder (leftSize, rightSize)
            

            largerAmount :: Int
            largerAmount = floor . (* k') $ largerSize / totalSize
            smallerAmount :: Int
            smallerAmount = k - largerAmount
            

            largerClusters  =  dendroToVectorClusters largerTree  largerAmount
            smallerClusters =  dendroToVectorClusters smallerTree smallerAmount
          in
            if smallerAmount == 0
            then largerClusters
            else largerClusters <> smallerClusters

{-
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

-}
