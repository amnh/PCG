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
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Data.Key
import           Data.List.NonEmpty (NonEmpty)
import           Prelude     hiding (zipWith)


extractNetworkMinimalDisplayTrees :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty EdgeSet
extractNetworkMinimalDisplayTrees (PDAG2 dag) = undefined
  where
    roots = (refs !) <$> rootRefs dag
    refs  = references dag
    

extractNetworkEdgeSet :: PhylogeneticDAG2 e n u v w x y z -> EdgeSet
extractNetworkEdgeSet (PDAG2 dag) = getEdges dag


extractBlocksMinimalEdgeSets :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty (CharacterBlock u v w x y z, NonEmpty EdgeSet)
extractBlocksMinimalEdgeSets (PDAG2 dag) = undefined
  where
    roots = (refs !) <$> rootRefs dag
    refs  = references dag
    
