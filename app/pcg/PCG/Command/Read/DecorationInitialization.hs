----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.DecorationInitialization
-- Copyright   :  () 2015-2015 Ward Wheeler
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

module PCG.Command.Read.DecorationInitialization where

import Analysis.Scoring
import Bio.Graph
import Bio.Graph.PhylogeneticDAG (pruneEdgeSet)


initializeDecorations2 :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
initializeDecorations2 (PhylogeneticSolution forests) =
    PhylogeneticSolution $ fmap decorateAndPruneEdges <$> forests
  where
    decorateAndPruneEdges dag
      | null extraEdges = result
      | otherwise       = performDecoration $ pruneEdgeSet extraEdges dag
      where
        extraEdges = unusedNetworkEdges postorderState
        (postorderState, result) = performPostorderDecoration dag
