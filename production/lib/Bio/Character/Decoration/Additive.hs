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
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasIsLeaf(..)
  , HasCharacterCost(..)
  , HasPreliminaryInterval(..)
  , HasFinalInterval(..)
  , HasChildPrelimIntervals(..)
  , RangedExtensionPreorder(..)
  , RangedExtensionPostorder(..)
  , RangedCharacterDecoration()
  , RangedDecorationOptimization()
  , RangedPostorderDecoration()
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Additive.Internal
import Bio.Character.Decoration.Shared
