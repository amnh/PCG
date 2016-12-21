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
  ( AdditiveOptimizationDecoration(AdditiveOptimizationDecoration)
  , AdditiveCharacterDecoration()
  , DiscreteExtensionAdditiveDecoration(..)
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasIsLeaf(..)
  , HasMinCost(..)
  , HasPreliminaryInterval(..)
  , HasChildPrelimIntervals(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Additive.Internal

