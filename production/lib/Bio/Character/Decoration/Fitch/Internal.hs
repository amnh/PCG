-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Fitch.Internal where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Fitch.Class
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data FitchDecorationInitial f
   = FitchDecorationInitial
   { fitchDecorationInitialCharacter :: f
   , metadata                        :: DiscreteCharacterMetadataDec f
   }


-- | (✔)
instance HasDiscreteCharacter (FitchDecorationInitial f) f where

    discreteCharacter = lens fitchDecorationInitialCharacter (\e x -> e { fitchDecorationInitialCharacter = x })


-- | (✔)
instance HasCharacterAlphabet (FitchDecorationInitial f) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (FitchDecorationInitial f) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (FitchDecorationInitial f) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance (EncodableStreamElement f) => HasCharacterTransitionCostMatrix (FitchDecorationInitial f) (f -> f -> (f, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }
        

-- | (✔)
instance HasCharacterWeight (FitchDecorationInitial f) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (FitchDecorationInitial f) where

-- | (✔)
instance (EncodableStreamElement f) => DiscreteCharacterMetadata (FitchDecorationInitial f) f where


-- | (✔)
instance EncodableStaticCharacter f => DiscreteCharacterDecoration (FitchDecorationInitial f) f where 

-- | (✔)
instance EncodableStaticCharacter f => SimpleDiscreteCharacterDecoration (FitchDecorationInitial f) f where 

    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        FitchDecorationInitial
        { fitchDecorationInitialCharacter = g symbolSet
        , metadata                        = discreteMetadata name weight alphabet tcm
        }    


-- | (✔)
instance EncodableStaticCharacter f => FitchCharacterDecoration (FitchDecorationInitial f) f where 
