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

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Clustering.Hierarchical where

import Bio.Sequence
import Bio.Graph.LeafSet
import Bio.Graph.Node
import Bio.Graph.Constructions
import AI.Clustering.Hierarchical
import Data.DList (DList)
import Data.Vector
import qualified Data.List.NonEmpty as NE
import qualified Data.DList as DL (toList)
import Analysis.Clustering.Metric
import Control.Lens
import Data.Coerce
import Data.Monoid (Sum(..))
import Data.NodeLabel
import Bio.Graph.PhylogeneticDAG


clusterLeaves
  :: constraint ???
  => ???
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
clusterShuffle meta leafSet opt = clusteredLeafSet

  where
    leafSetVector :: Vector CharacterNode
    leafSetVector = fromLeafSet leafSet

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
    clusteredLeafSet = coerce . DL.toList $ dendroToList dendro



dendroToList :: Dendrogram a -> DList a
dendroToList = \case
  Leaf a -> pure a
  Branch _ _ l r -> dendroToList l <> dendroToList r
