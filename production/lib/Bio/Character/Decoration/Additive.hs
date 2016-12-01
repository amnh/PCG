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
  ( AdditiveDecorationInitial()
  , AdditiveCharacterDecoration()
  , GeneralCharacterMetadata()
  , DiscreteCharacterMetadata()
  , DiscreteCharacterDecoration()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterSymbolTransitionCostMatrixGenerator(..)
  , HasCharacterTransitionCostMatrix(..)
  , HasCharacterWeight(..)
  , HasDiscreteCharacter(..)
  ) where

import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Additive.Internal

