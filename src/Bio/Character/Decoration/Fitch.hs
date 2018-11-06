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
  ( DiscreteCharacterDecoration()
  , DiscreteCharacterMetadata()
  , DiscreteExtensionFitchDecoration(..)
  , FitchCharacterDecoration()
  , FitchOptimizationDecoration(FitchOptimizationDecoration)
  , GeneralCharacterMetadata()
  -- * Lenses
  , GetSymbolChangeMatrix(..)
  , GetTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  , HasFinalMedian(..)
  , HasIsLeaf(..)
  , HasCharacterCost(..)
  , HasPreliminaryMedian(..)
  , HasChildMedians(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Decoration.Fitch.Internal
import Bio.Character.Decoration.Shared
