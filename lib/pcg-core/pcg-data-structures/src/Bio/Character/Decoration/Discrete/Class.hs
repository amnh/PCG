----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Discrete
-- Copyright   :  (c) 2015-2015 Ward Wheeler
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

import Control.Lens
import Bio.Character.Encodable


-- |
-- A 'Lens' for the 'discreteCharacter' field
class HasDiscreteCharacter s a | s -> a where

    discreteCharacter :: Lens' s a
    {-# MINIMAL discreteCharacter #-}


-- | (✔)
class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      ) => DiscreteCharacterDecoration s a | s -> a where


-- | (✔)
class DiscreteCharacterDecoration s a => SimpleDiscreteCharacterDecoration s a | s -> a where

    toDiscreteCharacterDecoration :: (x -> a) -> x -> s
    {-# MINIMAL toDiscreteCharacterDecoration #-}
