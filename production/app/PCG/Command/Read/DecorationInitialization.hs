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

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Read.DecorationInitialization where

import Analysis.Scoring
import Bio.Graph
import Bio.Graph.PhylogeneticDAG


initializeDecorations2 :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
initializeDecorations2 = scoreSolution
