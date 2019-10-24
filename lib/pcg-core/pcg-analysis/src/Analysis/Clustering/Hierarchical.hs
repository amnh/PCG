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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis.Clustering.Hierarchical where

import           AI.Clustering.Hierarchical
import           Analysis.Clustering.Metric
import           Bio.Graph.Constructions
import           Bio.Graph.LeafSet
import           Bio.Graph.Node             (HasSequenceDecoration (..))
import           Bio.Sequence
import           Control.Lens
import           Data.Coerce
import           Data.DList                 (DList)
import           Data.Monoid                (Sum (..))
import           Data.Vector                hiding (length, toList)
import qualified Data.Vector.NonEmpty       as NE
import           Prelude                    hiding (length)
import           VectorBuilder.Builder      (Builder)
import qualified VectorBuilder.Builder      as VB
import           VectorBuilder.Vector       (build)

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
      hclust opt leafSetVector distance


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
clusterIntoGroups meta leaves link =
    dendroToVectorClusters dendro
  where
    dendro = clusterLeaves meta leaves link

clusterIntoCuts
  :: (Applicative f, Foldable f)
  => MetadataSequence m
  -> LeafSet (DecoratedCharacterNode f)
  -> Linkage
  -> Double
  -> NE.Vector (NE.Vector (DecoratedCharacterNode f))
clusterIntoCuts meta leaves link =
    cutCluster dendro
  where
    dendro = clusterLeaves meta leaves link


dendroToList :: Dendrogram a -> DList a
dendroToList = \case
  Leaf a -> pure a
  Branch _ _ l r -> dendroToList l <> dendroToList r


dendroToVector
  :: Dendrogram a
  -> Vector a
dendroToVector =
    build . vectorBuilder
  where
    vectorBuilder :: Dendrogram a -> Builder a
    vectorBuilder = \case
      Leaf a -> VB.singleton a
      Branch _ _ d1 d2 -> vectorBuilder d1 <> vectorBuilder d2


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
dendroToVectorClusters numberOfClusters i =
      NE.unsafeFromVector
    . build
    $ vectorBuilder numberOfClusters i
  where
    vectorBuilder :: Dendrogram a -> Int -> Builder (NE.Vector a)
    vectorBuilder _ 0 = error "Cannot return zero clusters"
    vectorBuilder d n = case d of
      Leaf a -> VB.singleton $ pure a
      b@(Branch totalNumber _ leftTree rightTree) ->
        case n of
          1 -> VB.singleton $ dendroToNonEmptyVector b
          k ->
            let
              leftSize, rightSize, totalSize, k' :: Double
              leftSize = fromIntegral $ size leftTree
              totalSize = fromIntegral totalNumber
              rightSize = fromIntegral $ size rightTree
              k' = fromIntegral k

              reOrder = if rightSize > leftSize then (\(x, y) -> (y, x)) else id
              (largerTree, smallerTree) = reOrder (leftTree, rightTree)
              (largerSize, _) = reOrder (leftSize, rightSize)


              largerAmount :: Int
              largerAmount = floor . (* k') $ largerSize / totalSize
              smallerAmount :: Int
              smallerAmount = k - largerAmount


              largerClusters  =  vectorBuilder largerTree  largerAmount
              smallerClusters =  vectorBuilder smallerTree smallerAmount
            in
              if smallerAmount == 0
              then largerClusters
              else largerClusters <> smallerClusters



cutCluster
  :: forall a
  . Dendrogram a
  -> Double
  -> NE.Vector (NE.Vector a)
cutCluster d f = case d of
  Leaf a  -> pure . pure $ a
  Branch _ dist _ _ ->
    let
      clusters :: [Dendrogram a]
      clusters = d `cutAt` (f * dist)
      clustersV :: [NE.Vector a]
      clustersV = fmap dendroToNonEmptyVector clusters
    in
      NE.unsafeFromVector . fromList $ clustersV
