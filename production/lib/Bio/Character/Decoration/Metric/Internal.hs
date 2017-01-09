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
import Data.TCM


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data MetricDecorationInitial c
   = MetricDecorationInitial
   { metricDecorationInitialCharacter :: c
   , metadata                         :: DiscreteCharacterMetadataDec c
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


-- | (✔)
instance HasCharacterSymbolTransitionCostMatrixGenerator (MetricDecorationInitial c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = metadata e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { metadata = metadata e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- | (✔)
instance HasCharacterTransitionCostMatrix (MetricDecorationInitial c) (c -> c -> (c, Int)) where

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


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (MetricDecorationInitial c) c where

    toDiscreteCharacterDecoration name weight alphabet tcm g symbolSet =
        MetricDecorationInitial
        { metricDecorationInitialCharacter = g symbolSet
        , metadata                         = discreteMetadata name weight alphabet tcm
        }


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (MetricDecorationInitial c) c where



-- |
-- A concrete type representing the results of performing Sankoff's algorithm.
data SankoffOptimizationDecoration c
   = SankoffOptimizationDecoration
   { sankoffDirectionalMins :: ([Word], [Word])
   , sankoffMinCostVector   ::  [Word]
   , sankoffMinCost         ::   Word
   , sankoffMetadataField   :: DiscreteCharacterMetadataDec c
   , sankoffCharacterField  :: c
   }


-- | (✔)
instance HasDiscreteCharacter (SankoffOptimizationDecoration c) c where

    discreteCharacter = lens sankoffCharacterField (\e x -> e { sankoffCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (SankoffOptimizationDecoration c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterAlphabet
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (SankoffOptimizationDecoration c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterName
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterName .~ x }


-- | (✔)
instance HasCharacterSymbolTransitionCostMatrixGenerator (SankoffOptimizationDecoration c) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { sankoffMetadataField = sankoffMetadataField e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- | (✔)
instance HasCharacterTransitionCostMatrix (SankoffOptimizationDecoration c) (c -> c -> (c, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterTCM
         setter e f = e { sankoffMetadataField = sankoffMetadataField e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (SankoffOptimizationDecoration c) Double where

    characterWeight = lens getter setter
      where
         getter e   = sankoffMetadataField e ^. characterWeight
         setter e x = e { sankoffMetadataField = sankoffMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasMinCostVector (SankoffOptimizationDecoration c) [Word] where

    minCostVector = lens sankoffMinCostVector (\e x -> e { sankoffMinCostVector = x })


-- | (✔)
instance HasDirectionalMinVector (SankoffOptimizationDecoration c) ([Word], [Word]) where

    directionalMinVector = lens sankoffDirectionalMins (\e x -> e { sankoffDirectionalMins = x })


-- | (✔)
instance HasMinCost (SankoffOptimizationDecoration c) Word where

    minCost = lens sankoffMinCost (\e x -> e { sankoffMinCost = x })


-- | (✔)
instance GeneralCharacterMetadata (SankoffOptimizationDecoration c) where


-- | (✔)
instance EncodableStreamElement c => DiscreteCharacterMetadata (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => MetricCharacterDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => SankoffDecoration (SankoffOptimizationDecoration c) c where


-- | (✔)
instance EncodableStaticCharacter c => DiscreteExtensionSankoffDecoration (SankoffOptimizationDecoration c) c where

--    extendToSankoff :: DiscreteCharacterDecoration x c => x -> [Word] -> ([Word], [Word]) -> Word -> s
    extendDiscreteToSankoff subDecoration costVector directionVector cost =

        SankoffOptimizationDecoration
        { sankoffDirectionalMins = directionVector
        , sankoffMinCostVector   = costVector
        , sankoffMinCost         = cost
        , sankoffMetadataField   = metadataValue
        , sankoffCharacterField  = subDecoration ^. discreteCharacter
        }
      where
        alphabetValue = subDecoration ^. characterAlphabet
        tcmValue      = generate (length alphabetValue) (uncurry $ subDecoration ^. characterSymbolTransitionCostMatrixGenerator)
        metadataValue =
          discreteMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            <*> const alphabetValue
            <*> const tcmValue
            $ subDecoration
