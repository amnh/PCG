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
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data NonMetricDecorationInitial c
   = NonMetricDecorationInitial
   { nonMetricDecorationInitialCharacter :: c
   , metadata                        :: DiscreteCharacterMetadataDec c
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
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (NonMetricDecorationInitial c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance EncodableStreamElement c => HasCharacterTransitionCostMatrix (NonMetricDecorationInitial c) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }
        

-- | (✔)
instance HasCharacterWeight (NonMetricDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (NonMetricDecorationInitial c) where

-- | (✔)
instance EncodableStreamElement c => DiscreteCharacterMetadata (NonMetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (NonMetricDecorationInitial c) c where 
    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        NonMetricDecorationInitial
        { nonMetricDecorationInitialCharacter = g alphabet symbolSet
        , metadata                           = discreteMetadata name weight alphabet tcm
        }    


-- | (✔)
instance EncodableStaticCharacter c => NonMetricCharacterDecoration (NonMetricDecorationInitial c) c where
