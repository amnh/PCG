------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.NetworkEdgeQuantification
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.PhyloGraphPrime.PhylogeneticDAG.NetworkEdgeQuantification where

import           Bio.Sequence.Block (CharacterBlock)
import           Bio.PhyloGraphPrime.EdgeSet
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Data.Key
import           Data.List.NonEmpty (NonEmpty, transpose)
import           Prelude     hiding (zipWith)


extractNetworkMinimalDisplayTrees :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty (NetworkDisplayEdgeSet (Int, Int))
extractNetworkMinimalDisplayTrees (PDAG2 dag) = rootTransformation rootResolutions
  where
    -- We transpose the resolutions of the root nodes by transposing the
    -- NonEmpty structures, then collecting the edgesets of each root in each
    -- resolution together to create a NonEmpty collection of "network displays"
    --
    -- Since the number of roots in a DAG is fixed, deach network display will
    -- contain an equal number of elements in the network display.
    rootTransformation = fmap (fromEdgeSets . fmap subtreeEdgeSet) . transpose

    -- First we collect all resolutions for each root node
    rootResolutions = resolutions . nodeDecoration . (refs !) <$> rootRefs dag
    refs = references dag
    

extractNetworkEdgeSet :: PhylogeneticDAG2 e n u v w x y z -> EdgeSet (Int, Int)
extractNetworkEdgeSet (PDAG2 dag) = getEdges dag


extractBlocksMinimalEdgeSets :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty (CharacterBlock u v w x y z, NonEmpty (NetworkDisplayEdgeSet (Int,Int)))
extractBlocksMinimalEdgeSets (PDAG2 dag) = undefined
  where
    roots = (refs !) <$> rootRefs dag
    refs  = references dag
    
