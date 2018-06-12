-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.NonMetric.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.NonMetric.Internal where


import Bio.Character.Decoration.NonMetric.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
--import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens
import Data.Alphabet


-- |
-- An abstract initial dynamic character decoration with a polymorphic character type.
data NonMetricDecorationInitial c
   = NonMetricDecorationInitial
   { nonMetricDecorationInitialCharacter :: c
   , metadata                            :: {-# UNPACK #-} !(DiscreteWithTCMCharacterMetadataDec c)
   }


-- | (✔)
instance HasDiscreteCharacter (NonMetricDecorationInitial c) c where

    discreteCharacter = lens nonMetricDecorationInitialCharacter (\e x -> e { nonMetricDecorationInitialCharacter = x })


-- | (✔)
instance HasCharacterAlphabet (NonMetricDecorationInitial c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (NonMetricDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolChangeMatrix' field
instance HasSymbolChangeMatrix (NonMetricDecorationInitial c) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
         getter e   = metadata e ^. symbolChangeMatrix
         setter e f = e { metadata = metadata e & symbolChangeMatrix .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance HasTransitionCostMatrix (NonMetricDecorationInitial c) (c -> c -> (c, Word)) where

    transitionCostMatrix = lens getter setter
      where
         getter e   = metadata e ^. transitionCostMatrix
         setter e f = e { metadata = metadata e & transitionCostMatrix .~ f }


-- | (✔)
instance HasCharacterWeight (NonMetricDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (NonMetricDecorationInitial c) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . metadata


-- | (✔)
instance DiscreteCharacterMetadata (NonMetricDecorationInitial c) where

    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . metadata


-- | (✔)
instance EncodableStreamElement c => DiscreteWithTcmCharacterMetadata (NonMetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (NonMetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (NonMetricDecorationInitial c) c where
    toDiscreteCharacterDecoration name weight alphabet scm g symbolSet =
        NonMetricDecorationInitial
        { nonMetricDecorationInitialCharacter = g symbolSet
        , metadata                            = discreteMetadataWithTCM name weight alphabet scm
        }


-- | (✔)
instance EncodableStaticCharacter c => NonMetricCharacterDecoration (NonMetricDecorationInitial c) c where
