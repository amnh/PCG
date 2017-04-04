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
  ( ContinuousOptimizationDecoration()
  , ContinuousPostorderDecoration()
  , ContinuousCharacterDecoration()
  , DiscreteExtensionContinuousDecoration(..)
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasIsLeaf(..)
  , HasCharacterCost(..)
  , HasPreliminaryInterval(..)
  , HasChildPrelimIntervals(..)
  , HasIntervalCharacter(..)
  , continuousDecorationInitial
  , ContinuousDecorationInitial()
--  , ContinuousCharacter()
--  , GeneralCharacterMetadata()
--  , HasCharacterName(..)
--  , HasCharacterWeight(..)
  , HasContinuousCharacter(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Continuous.Internal
import Bio.Character.Decoration.Shared
