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
import Bio.Metadata.Continuous
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet
--import Data.Bits
import Data.TCM
--import Data.Double




-- |
-- An abstract initial continuous character decoration with a polymorphic character
-- type.
data ContinuousDecorationInitial c
   = ContinuousDecorationInitial
   { continuousDecorationInitialCharacter :: c
   , continuousMetadataField              :: ContinuousCharacterMetadataDec
   }


-- | A smart constructor for a continuous character.
continuousDecorationInitial :: CharacterName -> (x -> c) -> x -> ContinuousDecorationInitial c
continuousDecorationInitial name f v =
    ContinuousDecorationInitial
    { continuousDecorationInitialCharacter = f v
    , continuousMetadataField              = continuousMetadata name 1
    }


-- |
-- A newtype wrapper for a possibly missing continuous.
newtype ContinuousChar = CC (Maybe Double)
  deriving (Eq,Ord)


-- | (✔)
instance Show ContinuousChar where

    show (CC  Nothing) = "?"
    show (CC (Just x)) = show x


-- | (✔)
instance Show c => Show (ContinuousDecorationInitial c) where

    show = show . (^. continuousCharacter)


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (ContinuousDecorationInitial c) where

    isMissing = isMissing . (^. continuousCharacter)

    toMissing x = x & continuousCharacter %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter ContinuousChar where

    {-# INLINE toMissing #-}
    toMissing = const $ CC Nothing

    {-# INLINE isMissing #-}
    isMissing (CC Nothing) = True
    isMissing _            = False


-- | (✔)
instance ContinuousCharacter ContinuousChar where

    toContinuousCharacter = CC . fmap (fromRational . toRational)


-- | (✔)
instance HasCharacterName (ContinuousDecorationInitial c) CharacterName where

    characterName = lens getter setter
      where
         getter e   = continuousMetadataField e ^. characterName
         setter e x = e { continuousMetadataField = continuousMetadataField e &  characterName .~ x }


-- | (✔)
instance HasCharacterWeight (ContinuousDecorationInitial c) Double where

    characterWeight = lens getter setter
      where
         getter e   = continuousMetadataField e ^. characterWeight
         setter e x = e { continuousMetadataField = continuousMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasContinuousCharacter (ContinuousDecorationInitial c) c where

    continuousCharacter = lens continuousDecorationInitialCharacter $ \e x -> e { continuousDecorationInitialCharacter = x }


-- | (✔)
instance GeneralCharacterMetadata (ContinuousDecorationInitial d) where


-- | (✔)
instance ContinuousCharacter c => ContinuousDecoration (ContinuousDecorationInitial c) c where




data ContinuousOptimizationDecoration a
   = ContinuousOptimizationDecoration
   { continuousMinCost              :: Double
   , continuousPreliminaryInterval  :: (Double, Double)
   , continuousChildPrelimIntervals :: ((Double, Double), (Double, Double))
   , continuousIsLeaf               :: Bool
   , continuousCharacterField       :: a
   , continuousMetadataField        :: ContinuousCharacterMetadataDec
   }


-- | (✔)
instance HasContinuousCharacter (ContinuousOptimizationDecoration c) c where

    continuousCharacter = lens continuousCharacterField $ \e x -> e { continuousCharacterField = x }


-- | (✔)
instance HasCharacterName (ContinuousOptimizationDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   = continuousMetadataField e ^. characterName
         setter e x = e { continuousMetadataField = continuousMetadataField e &  characterName .~ x }


-- | (✔)
instance HasCharacterWeight (ContinuousOptimizationDecoration a) Double where

    characterWeight = lens getter setter
      where
         getter e   = continuousMetadataField e ^. characterWeight
         setter e x = e { continuousMetadataField = continuousMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (ContinuousOptimizationDecoration a) Bool where

    isLeaf = lens continuousIsLeaf (\e x -> e { continuousIsLeaf = x })


-- | (✔)
instance HasCharacterCost (ContinuousOptimizationDecoration a) Double where

    characterCost = lens continuousMinCost (\e x -> e { continuousMinCost = x })


-- | (✔)
instance HasPreliminaryInterval (ContinuousOptimizationDecoration a) (Double, Double) where

    preliminaryInterval = lens continuousPreliminaryInterval (\e x -> e { continuousPreliminaryInterval = x })


-- | (✔)
instance HasChildPrelimIntervals (ContinuousOptimizationDecoration a) ((Double, Double),(Double, Double)) where

    childPrelimIntervals = lens continuousChildPrelimIntervals (\e x -> e { continuousChildPrelimIntervals = x })


-- | (✔)
instance GeneralCharacterMetadata (ContinuousOptimizationDecoration a) where


{-
-- | (✔)
instance EncodableStreamElement a => DiscreteCharacterMetadata (ContinuousOptimizationDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (ContinuousOptimizationDecoration a) a where
-}


-- | (✔)
instance ContinuousCharacter a => ContinuousDecoration (ContinuousOptimizationDecoration a) a where


-- | (✔)
instance ContinuousCharacter a => ContinuousCharacterDecoration (ContinuousOptimizationDecoration a) a where


{-
-- | (✔)
instance EncodableStaticCharacter a => ContinuousAdditiveHybridDecoration (ContinuousOptimizationDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteExtensionContinuousDecoration (ContinuousOptimizationDecoration a) a where

    extendDiscreteToContinuous subDecoration cost prelimInterval childMedianTup isLeafVal =

        ContinuousOptimizationDecoration
        { continuousChildPrelimIntervals = childMedianTup
        , continuousIsLeaf               = isLeafVal
        , continuousMinCost              = cost
        , continuousMetadataField        = metadataValue
        , continuousPreliminaryInterval  = prelimInterval
        , continuousCharacterField       = subDecoration ^. discreteCharacter
        }
      where
        metadataValue =
          continuousMetadata
            <$> (^. characterName)
            <*> (^. characterWeight)
            $ subDecoration
-}
