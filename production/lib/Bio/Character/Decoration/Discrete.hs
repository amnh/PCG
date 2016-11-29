-----------------------------------------------------------------------------
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

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Discrete where


import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Control.Lens


-- |
-- A 'Lens' for the 'discreteChararcter' field
class HasDiscreteCharacter s a | s -> a where

    discreteChararcter :: Lens' s a
    {-# MINIMAL discreteChararcter #-}


class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      , DiscreteCharacterMetadata s a
      ) => DiscreteCharacterDecoration s a | s -> a where 
