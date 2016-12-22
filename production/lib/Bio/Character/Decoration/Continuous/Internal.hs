-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Continuous.Internal where


import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet
--import Data.Bits
import Data.TCM
--import Data.Double


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data ContinuousOptimizationDecoration a
   = ContinuousOptimizationDecoration
   { additiveMinCost              :: Double
   , additivePreliminaryInterval  :: (Double, Double)
   , additiveChildPrelimIntervals :: ((Double, Double), (Double, Double))
   , additiveIsLeaf               :: Bool
   , additiveCharacterField       :: a
   , additiveMetadataField        :: DiscreteCharacterMetadataDec a
   }


-- | (✔)
instance HasDiscreteCharacter (ContinuousOptimizationDecoration a) a where

    discreteCharacter = lens additiveCharacterField (\e x -> e { additiveCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (ContinuousOptimizationDecoration a) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterAlphabet
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (ContinuousOptimizationDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterName
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterName .~ x }


-- | (✔)
instance HasCharacterSymbolTransitionCostMatrixGenerator (ContinuousOptimizationDecoration a) (Int -> Int -> Int) where

    characterSymbolTransitionCostMatrixGenerator = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterSymbolTransitionCostMatrixGenerator
         setter e f = e { additiveMetadataField = additiveMetadataField e &  characterSymbolTransitionCostMatrixGenerator .~ f }


-- | (✔)
instance HasCharacterTransitionCostMatrix (ContinuousOptimizationDecoration a) (a -> a -> (a, Int)) where

    characterTCM = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterTCM
         setter e f = e { additiveMetadataField = additiveMetadataField e &  characterTCM .~ f }


-- | (✔)
instance HasCharacterWeight (ContinuousOptimizationDecoration a) Double where

    characterWeight = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterWeight
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterWeight .~ x }

-- | (✔)
instance HasIsLeaf (ContinuousOptimizationDecoration a) Bool where

    isLeaf = lens additiveIsLeaf (\e x -> e { additiveIsLeaf = x })

-- | (✔)
instance HasMinCost (ContinuousOptimizationDecoration a) Double where

    minCost = lens additiveMinCost (\e x -> e { additiveMinCost = x })

-- | (✔)
instance HasPreliminaryInterval (ContinuousOptimizationDecoration a) (Double, Double) where

    preliminaryInterval = lens additivePreliminaryInterval (\e x -> e { additivePreliminaryInterval = x })

-- | (✔)
instance HasChildPrelimIntervals (ContinuousOptimizationDecoration a) ((Double, Double),(Double, Double)) where

    childPrelimIntervals = lens additiveChildPrelimIntervals (\e x -> e { additiveChildPrelimIntervals = x })


-- | (✔)
instance GeneralCharacterMetadata (ContinuousOptimizationDecoration a) where

-- | (✔)
instance EncodableStreamElement a => DiscreteCharacterMetadata (ContinuousOptimizationDecoration a) a where

-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (ContinuousOptimizationDecoration a) a where

-- | (✔)
instance EncodableStaticCharacter a => ContinuousCharacterDecoration (ContinuousOptimizationDecoration a) a where

-- | (✔)
instance EncodableStaticCharacter a => ContinuousDecoration (ContinuousOptimizationDecoration a) a where

-- | (✔)
instance EncodableStaticCharacter a => DiscreteExtensionContinuousDecoration (ContinuousOptimizationDecoration a) a where

    extendDiscreteToContinuous subDecoration cost prelimInterval childMedianTup isLeafVal =

        ContinuousOptimizationDecoration
        { additiveChildPrelimIntervals = childMedianTup
        , additiveIsLeaf               = isLeafVal
        , additiveMinCost              = cost
        , additiveMetadataField        = metadataValue
        , additivePreliminaryInterval  = prelimInterval
        , additiveCharacterField       = subDecoration ^. discreteCharacter
        }
      where
        metadataValue =
          discreteMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            $ subDecoration

