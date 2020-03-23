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

module Bio.Graph.PhylogeneticDAG
  ( PhylogeneticFreeDAG(..)
  , PhylogeneticDAG(..)
  , PostorderContextualData(..)
  , HasVirtualNodeMapping(..)
  , HasPhylogeneticForest(..)
  , HasColumnMetadata(..)
  , EdgeReference
  , assignOptimalDynamicCharacterRootEdges
  , assignPunitiveNetworkEdgeCost
  , generateLocalResolutions
  , postorderSequence'
  , preorderFromRooting
  , preorderSequence
  , renderSummary
  , setEdgeSequences
  , getDotContextWithBaseAndIndex
  , setDefaultMetadata
  , pruneEdgeSet
  -- * Substitution functions
  , getNamedContext
  , substituteDAGs
  ) where

import           Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
import           Bio.Graph.PhylogeneticDAG.Postorder
import           Bio.Graph.PhylogeneticDAG.Preorder
import           Bio.Graph.PhylogeneticDAG.Substitute
import           Bio.Graph.ReferenceDAG.Internal
