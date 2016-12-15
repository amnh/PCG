-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Additive.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Additive.Internal where


import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data AdditiveDecorationInitial c
   = AdditiveDecorationInitial
   { additiveDecorationInitialCharacter :: c
   , metadata                           :: DiscreteCharacterMetadataDec c
   }


-- | (✔)
instance HasDiscreteCharacter (AdditiveDecorationInitial c) c where

    discreteCharacter = lens additiveDecorationInitialCharacter (\e x -> e { additiveDecorationInitialCharacter = x })


-- | (✔)
instance HasCharacterAlphabet (AdditiveDecorationInitial c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditiveDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- | (✔)
instance HasCharacterSymbolTransitionCostMatrixGenerator (AdditiveDecorationInitial c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- | (✔)
instance EncodableStreamElement c => HasCharacterTransitionCostMatrix (AdditiveDecorationInitial c) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (AdditiveDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (AdditiveDecorationInitial c) where

-- | (✔)
instance EncodableStreamElement c => DiscreteCharacterMetadata (AdditiveDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (AdditiveDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (AdditiveDecorationInitial c) c where
    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        AdditiveDecorationInitial
        { additiveDecorationInitialCharacter = g symbolSet
        , metadata                           = discreteMetadata name weight alphabet tcm
        }


-- | (✔)
instance EncodableStaticCharacter c => AdditiveCharacterDecoration (AdditiveDecorationInitial c) c where
