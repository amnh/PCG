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
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Discrete
import Bio.Metadata.DiscreteWithTCM
import Control.Lens
import Data.Alphabet
--import Data.Bits
import Data.TCM
import Data.Semigroup
--import Data.Word


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data AdditiveOptimizationDecoration a
   = AdditiveOptimizationDecoration
   { additiveMinCost              :: Word
   , additivePreliminaryInterval  :: (Word, Word)
   , additiveChildPrelimIntervals :: ((Word, Word), (Word, Word))
   , additiveIsLeaf               :: Bool
   , additiveCharacterField       :: a   -- TODO: do I need this?
   , additiveMetadataField        :: DiscreteCharacterMetadataDec
   }


instance EncodableStreamElement c => Show (AdditiveOptimizationDecoration c) where

    show c = unlines
        [ "Cost = "                <> show (c ^. characterCost)
        , "Is Leaf Node?       : " <> show (c ^. isLeaf)
        , "Discrete Character  : " <> showDiscreteCharacterElement c
        , "Preliminary Interval: " <> show (additivePreliminaryInterval c)
        , "Child      Intervals: " <> show (additiveChildPrelimIntervals c)
        ]


-- | (✔)
instance HasDiscreteCharacter (AdditiveOptimizationDecoration a) a where

    discreteCharacter = lens additiveCharacterField (\e x -> e { additiveCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (AdditiveOptimizationDecoration a) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterAlphabet
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditiveOptimizationDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterName
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterName .~ x }


-- | (✔)
instance HasSymbolChangeMatrix (AdditiveOptimizationDecoration a) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
        getter = const $ \i j -> max i j - min i j
        setter = const


-- | (✔)
instance HasTransitionCostMatrix (AdditiveOptimizationDecoration a) (a -> a -> (a, Word)) where


    -- NOTE: This probably isn't sound
    transitionCostMatrix = lens getter setter
      where
        getter = error "Please don't use lens accessor operations over 'transitionCostMatrix' on a AdditiveOptimizationDecoration."
        setter = const


-- | (✔)
instance HasCharacterWeight (AdditiveOptimizationDecoration a) Double where

    characterWeight = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterWeight
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (AdditiveOptimizationDecoration a) Bool where

    isLeaf = lens additiveIsLeaf (\e x -> e { additiveIsLeaf = x })


-- | (✔)
instance HasCharacterCost (AdditiveOptimizationDecoration a) Word where

    characterCost = lens additiveMinCost (\e x -> e { additiveMinCost = x })


-- | (✔)
instance HasPreliminaryInterval (AdditiveOptimizationDecoration a) (Word, Word) where

    preliminaryInterval = lens additivePreliminaryInterval (\e x -> e { additivePreliminaryInterval = x })


-- | (✔)
instance HasChildPrelimIntervals (AdditiveOptimizationDecoration a) ((Word, Word),(Word, Word)) where

    childPrelimIntervals = lens additiveChildPrelimIntervals (\e x -> e { additiveChildPrelimIntervals = x })


-- | (✔)
instance GeneralCharacterMetadata (AdditiveOptimizationDecoration a) where

  
-- | (✔)
instance DiscreteCharacterMetadata (AdditiveOptimizationDecoration a) where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteWithTcmCharacterMetadata (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditiveOptimizationDecoration a) a where

  
-- | (✔)
instance EncodableStaticCharacter a => AdditiveCharacterDecoration (AdditiveOptimizationDecoration a) a where

  
-- | (✔)
instance EncodableStaticCharacter a => AdditiveDecoration (AdditiveOptimizationDecoration a) a where

  
-- | (✔)
instance EncodableStaticCharacter a => DiscreteExtensionAdditiveDecoration (AdditiveOptimizationDecoration a) a where

    extendDiscreteToAdditive subDecoration cost prelimInterval childMedianTup isLeafVal =

        AdditiveOptimizationDecoration
        { additiveChildPrelimIntervals = childMedianTup
        , additiveIsLeaf               = isLeafVal
        , additiveMinCost              = cost
        , additiveMetadataField        = metadataValue
        , additivePreliminaryInterval  = prelimInterval
        , additiveCharacterField       = subDecoration ^. discreteCharacter -- TODO: do I need this?
        }
      where
        metadataValue =
          discreteMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            <*> (^. characterAlphabet)
            $ subDecoration

