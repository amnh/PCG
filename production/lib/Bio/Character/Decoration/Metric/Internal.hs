-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Metric.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Metric.Internal where


import Bio.Character.Decoration.Metric.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data MetricDecorationInitial c
   = MetricDecorationInitial
   { metricDecorationInitialCharacter :: c
   , metadata                        :: DiscreteCharacterMetadataDec c
   }


-- | (✔)
instance HasDiscreteCharacter (MetricDecorationInitial c) c where

    discreteCharacter = lens metricDecorationInitialCharacter (\e x -> e { metricDecorationInitialCharacter = x })


-- | (✔)
instance HasCharacterAlphabet (MetricDecorationInitial c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = metadata e ^. characterAlphabet
         setter e x = e { metadata = metadata e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (MetricDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = metadata e ^. characterName
         setter e x = e { metadata = metadata e &  characterName .~ x }


-- |
-- A 'Lens' for the 'symbolicTCMGenerator' field
instance HasCharacterSymbolTransitionCostMatrixGenerator (MetricDecorationInitial c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
instance EncodableStreamElement c => HasCharacterTransitionCostMatrix (MetricDecorationInitial c) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = metadata e ^. characterTCM
         setter e f = e { metadata = metadata e &  characterTCM .~ f }
        

-- | (✔)
instance HasCharacterWeight (MetricDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = metadata e ^. characterWeight
         setter e x = e { metadata = metadata e &  characterWeight .~ x }


-- | (✔)
instance GeneralCharacterMetadata (MetricDecorationInitial c) where

-- | (✔)
instance EncodableStreamElement c => DiscreteCharacterMetadata (MetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (MetricDecorationInitial c) c where 
    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        MetricDecorationInitial
        { metricDecorationInitialCharacter = g alphabet symbolSet
        , metadata                           = discreteMetadata name weight alphabet tcm
        }    


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (MetricDecorationInitial c) c where
