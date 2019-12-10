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
  , CharacterNode
  , DecoratedCharacterResult
  , DecoratedCharacterNode
  , EdgeAnnotation
  , FinalDecorationDAG
  , FinalCharacterNode
  , FinalCharacterSequence
  , FinalMetadata
  , FinalReferenceVector
  , GlobalSettings
  , GraphState
  , HasPhylogeneticForest(..)
  , PhylogeneticFreeDAG(..)
  , PhylogeneticDAG(..)
  , PhylogeneticDAGish(..)
  , PhylogeneticForest(..)
  , PhylogeneticSolution(..)
  , PreorderDecorationDAG
  , PostorderDecorationDAG
  , SearchState
  , TopologicalResult
  , UndecoratedReferenceDAG
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
  , extractPhylogeneticForest
  , extractReferenceDAG
  , generateLocalResolutions
  , phylogeneticForests
  , postorderSequence'
  , preorderFromRooting
  , preorderSequence
  , renderSummary
  , reifiedSolution
  , rootCosts
  , setEdgeSequences
  -- * Mapping over networks
  , edgePreorderMap
  , edgePostorderMap
  , edgePreorderFold
  , edgePostorderFold
  , nodePreorderMap
  , nodePostorderMap
  , nodePreorderFold
  , nodePostorderFold
  -- * Substitution functions
  , getNamedContext
  , substituteDAGs
  ) where


import Bio.Graph.Constructions
import Bio.Graph.PhylogeneticDAG
import Bio.Graph.PhylogeneticDAG.Reification
import Bio.Graph.Solution
