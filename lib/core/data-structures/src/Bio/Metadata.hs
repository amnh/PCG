-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Wrapper for all the metadata types
--
-----------------------------------------------------------------------------

module Bio.Metadata
  (
  -- * General Metadata Shared by All Characters
    GeneralCharacterMetadata(..)
  , GeneralCharacterMetadataDec()
  , generalMetadata
  -- * Continuous Character Metadata
  , ContinuousCharacterMetadataDec()
  , continuousMetadata
  -- * Discrete Character Metadata
  , DiscreteCharacterMetadataDec()
  , DiscreteCharacterMetadata(..)
  , discreteMetadata
  -- * Discrete Character with TCM Metadata
  , DiscreteWithTcmCharacterMetadata()
  , DiscreteWithTCMCharacterMetadataDec()
  , discreteMetadataFromTCM
  -- * Dynamic Character Metadata
  , DenseTransitionCostMatrix
  , DynamicCharacterMetadata(..)
  , DynamicCharacterMetadataDec()
  , MemoizedCostMatrix()
  , TraversalFoci
  , TraversalFocusEdge
  , TraversalTopology
  , dynamicMetadata
  , dynamicMetadataFromTCM
  , maybeConstructDenseTransitionCostMatrix
  -- * Lens fields
  , GetDenseTransitionCostMatrix(..)
  , GetSparseTransitionCostMatrix(..)
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , GetThreewayTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasTcmSourceFile(..)
  , HasTraversalFoci(..)
  ) where

import Bio.Metadata.Continuous
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic
import Bio.Metadata.General
