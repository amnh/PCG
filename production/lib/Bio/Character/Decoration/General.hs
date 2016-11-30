-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.General
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.General where


import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Control.Lens

import Bio.Metadata.CharacterName
import Data.Alphabet
import Data.TCM


-- |
-- A 'Lens' for the 'discreteCharacter' field
class HasDiscreteCharacter s a | s -> a where

    discreteCharacter :: Lens' s a
    {-# MINIMAL discreteCharacter #-}


class ( HasDiscreteCharacter s a
      , EncodableStaticCharacter a
      , DiscreteCharacterMetadata s a
      ) => DiscreteCharacterDecoration s a | s -> a where 

    toDiscreteCharacterDecoration :: CharacterName -> Double -> Alphabet String -> TCM-> (Alphabet String -> AmbiguityGroup String -> a) ->  AmbiguityGroup String -> s
    {-# MINIMAL toDiscreteCharacterDecoration #-}
    
