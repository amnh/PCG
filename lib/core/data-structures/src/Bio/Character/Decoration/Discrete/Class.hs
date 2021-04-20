----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Discrete
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Character.Decoration.Discrete.Class
  ( DiscreteCharacterDecoration()
  , HasDiscreteCharacter(..)
  , SimpleDiscreteCharacterDecoration(..)
  ) where

import Bio.Character.Encodable
import Control.Lens.Type       (Lens')


-- |
-- A 'Control.Lens.Type.Lens' for the 'discreteCharacter' field.
class HasDiscreteCharacter s a | s -> a where

    discreteCharacter :: Lens' s a
    {-# MINIMAL discreteCharacter #-}


-- |
-- Constraint for a the class of discrete character decorations.
class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      ) => DiscreteCharacterDecoration s a | s -> a where


-- |
-- Constructor class for producing a discrete character decoration.
class DiscreteCharacterDecoration s a => SimpleDiscreteCharacterDecoration s a | s -> a where

    toDiscreteCharacterDecoration :: (x -> a) -> x -> s
    {-# MINIMAL toDiscreteCharacterDecoration #-}
