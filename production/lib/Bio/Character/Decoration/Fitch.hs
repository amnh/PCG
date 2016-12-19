-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Fitch
  ( FitchCharacterDecoration()
  , FitchOptimizationDecoration(FitchOptimizationDecoration)
  , DiscreteExtensionFitchDecoration(..)
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
  , HasPreliminaryMedian(..)
  , HasChildMedians(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Decoration.Fitch.Internal

