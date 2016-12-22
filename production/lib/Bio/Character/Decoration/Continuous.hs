-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Decoration.Continuous
  ( ContinuousOptimizationDecoration(ContinuousOptimizationDecoration)
  , ContinuousCharacterDecoration()
  , DiscreteExtensionContinuousDecoration(..)
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
import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Continuous.Internal
import Bio.Character.Decoration.Shared
