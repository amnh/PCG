-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Dynamic
  ( DynamicDecorationInitial()
  , DirectOptimizationPostorderDecoration()
  , DynamicDecorationDirectOptimization()
  , SimpleDynamicDecoration()
  , DirectOptimizationDecoration()
  , ImpliedAlignmentDecoration()
  , DynamicCharacterDecoration(..)
  , DynamicDecorationDirectOptimizationPostorderResult()
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteWithTcmCharacterMetadata()
  , DynamicCharacterMetadata()
  , SimpleDynamicExtensionPostorderDecoration(..)
  , PostorderExtensionDirectOptimizationDecoration(..)
  , HasAlignmentContext(..)
  , HasAverageLength(..)
  , HasCharacterAlphabet(..)
  , HasCharacterCost(..)
  , HasCharacterLocalCost(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasEncoded(..)
  , HasImpliedAlignment(..)
  , HasSingleDisambiguation(..)
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasTraversalFoci(..)
  , GetSparseTransitionCostMatrix(..)
  , GetDenseTransitionCostMatrix(..)
  , PossiblyMissingCharacter(..)
  , AverageLength()
  , toAverageLength
  , getAverageLength
  , TraversalFoci
  , TraversalFocusEdge
  , TraversalTopology
  ) where

import Bio.Character.Decoration.Dynamic.Class
import Bio.Character.Decoration.Dynamic.Internal
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Bio.Metadata.Dynamic

