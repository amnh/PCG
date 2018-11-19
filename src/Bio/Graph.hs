------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG
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

module Bio.Graph
  ( CharacterResult
  , CharacterDAG
  , DecoratedCharacterResult
  , FinalDecorationDAG
  , GraphState
  , PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , PhylogeneticDAGish(..)
  , PhylogeneticForest(..)
  , PhylogeneticSolution(..)
  , PostorderDecorationDAG
  , SearchState
  , TopologicalResult
  , UnifiedBlock
  , UnifiedSequences
  , UnifiedCharacterBlock
  , UnifiedCharacterSequence
  , UnifiedContinuousCharacter
  , UnifiedDiscreteCharacter
  , UnifiedDynamicCharacter
  , UnifiedMetadataBlock
  , UnifiedMetadataSequence
  , UnReifiedCharacterDAG
  , assignOptimalDynamicCharacterRootEdges
  , assignPunitiveNetworkEdgeCost
  , extractSolution
  , extractReferenceDAG
  , generateLocalResolutions
  , postorderSequence'
  , preorderFromRooting
  , preorderSequence
  , renderSummary
  , reifiedSolution
  , rootCosts
  , phylogeneticForests
  -- * Mapping over networks
  , edgePreorderMap
  , edgePostorderMap
  , edgePreorderFold
  , edgePostorderFold
  , nodePreorderMap
  , nodePostorderMap
  , nodePreorderFold
  , nodePostorderFold
  ) where


import Bio.Graph.Constructions
import Bio.Graph.PhylogeneticDAG
import Bio.Graph.PhylogeneticDAG.Reification
import Bio.Graph.Solution
