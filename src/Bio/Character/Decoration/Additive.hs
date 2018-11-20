-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Additive
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Additive
  ( AdditivePostorderDecoration()
  , AdditiveOptimizationDecoration()
--  , AdditiveCharacterDecoration()
--  , AdditiveDecoration()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , GeneralCharacterMetadata()
  , RangedDecorationOptimization()
  , RangedPostorderDecoration()
  -- * Lenses
  , GetSymbolChangeMatrix(..)
  , GetPairwiseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterCost(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasChildPrelimIntervals(..)
  , HasDiscreteCharacter(..)
  , HasFinalInterval(..)
  , HasIntervalCharacter(..)
  , HasIsLeaf(..)
  , HasPreliminaryInterval(..)
  , RangedCharacterDecoration()
  , RangedExtensionPostorder(..)
  , RangedExtensionPreorder(..)
  ) where

import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Additive.Internal
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
