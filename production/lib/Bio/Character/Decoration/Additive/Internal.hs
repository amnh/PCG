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

-- We need this for the generalized type family derivation of Ranged instances.
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Range
import Data.Semigroup


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data AdditivePreorderDecoration a
   = AdditivePreorderDecoration
   { additiveCost                 :: Bound a
   , additivePreliminaryInterval  :: Range (Bound a)
   , additiveChildPrelimIntervals :: (Range (Bound a), Range (Bound a))
   , additiveIsLeaf               :: Bool
   , additiveCharacterField       :: a
   , additiveMetadataField        :: DiscreteCharacterMetadataDec
   }


instance
  ( EncodableStreamElement c
  , Show (Bound c)
  , Show (Range (Bound c))
  ) => Show (AdditivePreorderDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Discrete Character   : " <> showDiscreteCharacterElement c
        , "Preliminary Interval : " <> show (additivePreliminaryInterval c)
        , "Child       Intervals: " <> show (additiveChildPrelimIntervals c)
        ]


-- | (✔)
instance HasDiscreteCharacter (AdditivePreorderDecoration a) a where

    discreteCharacter = lens additiveCharacterField (\e x -> e { additiveCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (AdditivePreorderDecoration a) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterAlphabet
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditivePreorderDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterName
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterName .~ x }


-- | (✔)
instance HasSymbolChangeMatrix (AdditivePreorderDecoration a) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
        getter = const $ \i j -> max i j - min i j
        setter = const


-- | (✔)
instance HasTransitionCostMatrix (AdditivePreorderDecoration a) (a -> a -> (a, Word)) where


    -- NOTE: This probably isn't sound
    transitionCostMatrix = lens getter setter
      where
        getter = error "Please don't use lens accessor operations over 'transitionCostMatrix' on a AdditiveOptimizationDecoration."
        setter = const


-- | (✔)
instance HasCharacterWeight (AdditivePreorderDecoration a) Double where

    characterWeight = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterWeight
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (AdditivePreorderDecoration a) Bool where

    isLeaf = lens additiveIsLeaf (\e x -> e { additiveIsLeaf = x })


-- | (✔)
instance (Bound a ~ c) => HasCharacterCost (AdditivePreorderDecoration a) c where

    characterCost = lens additiveCost (\e x -> e { additiveCost = x })


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (AdditivePreorderDecoration a) (Range c) where

    preliminaryInterval = lens additivePreliminaryInterval (\e x -> e { additivePreliminaryInterval = x })


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (AdditivePreorderDecoration a) (Range c, Range c) where

    childPrelimIntervals = lens additiveChildPrelimIntervals (\e x -> e { additiveChildPrelimIntervals = x })


-- | (✔)
instance GeneralCharacterMetadata (AdditivePreorderDecoration a) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . additiveMetadataField


-- | (✔)
instance DiscreteCharacterMetadata (AdditivePreorderDecoration a) where

    extractDiscreteCharacterMetadata = additiveMetadataField


-- | (✔)
instance EncodableStaticCharacter a => DiscreteWithTcmCharacterMetadata (AdditivePreorderDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditivePreorderDecoration a) a where


{-
class ( RangedCharacterDecoration s c
      , HasCharacterCost s (Bound c)
      , HasChildPrelimIntervals s (Range (Bound c), Range (Bound c))
      , HasIsLeaf s Bool
      , HasPreliminaryInterval s (Range (Bound c))
      ) => RangedPostOrderDecoration s c | s -> c where


class ( RangedCharacterDecoration s c
      , HasFinalInterval s (Range (Bound c))
      ) => RangedDecorationOptimization s c | s -> c where 
-}
  

-- | (✔)
instance ( DiscreteCharacterMetadata   (AdditivePreorderDecoration a)
         , RangedPostorderDecoration   (AdditivePreorderDecoration a) a
         ) => RangedExtensionPostorder (AdditivePreorderDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =

        AdditivePreorderDecoration
        { additiveChildPrelimIntervals = childMedianTup
        , additiveIsLeaf               = isLeafVal
        , additiveCost                 = cost
        , additiveMetadataField        = extractDiscreteCharacterMetadata subDecoration
        , additivePreliminaryInterval  = prelimInterval
        , additiveCharacterField       = subDecoration ^. discreteCharacter
        }






data AdditiveOptimizationDecoration a
   = AdditiveOptimizationDecoration
   { additiveFinalInterval :: Range (Bound a)
   , preorderDecoration    :: AdditivePreorderDecoration a
   }


instance
  ( EncodableStreamElement c
  , Show (Bound c)
  , Show (Range (Bound c))
  ) => Show (AdditiveOptimizationDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost       )
        , "Is Leaf Node?        : " <> show (c ^. isLeaf              )
        , "Discrete Character   : " <> showDiscreteCharacterElement c
        , "Preliminary Interval : " <> show (c ^. preliminaryInterval )
        , "Child       Intervals: " <> show (c ^. childPrelimIntervals)
        , "Final       Interval : " <> show (c ^. finalInterval       )
        ]


-- | (✔)
instance HasDiscreteCharacter (AdditiveOptimizationDecoration a) a where

    {-# INLINE discreteCharacter #-}
    discreteCharacter = lens getter setter
      where
        getter e   =     preorderDecoration e ^. discreteCharacter
        setter e x = e { preorderDecoration = preorderDecoration e & discreteCharacter .~ x }


-- | (✔)
instance HasCharacterAlphabet (AdditiveOptimizationDecoration a) (Alphabet String) where

    {-# INLINE characterAlphabet #-}
    characterAlphabet = lens getter setter
      where
         getter e   =     preorderDecoration e ^. characterAlphabet
         setter e x = e { preorderDecoration = preorderDecoration e & characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditiveOptimizationDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   =     preorderDecoration e ^. characterName
         setter e x = e { preorderDecoration = preorderDecoration e & characterName .~ x }


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
         getter e   =     preorderDecoration e ^. characterWeight
         setter e x = e { preorderDecoration = preorderDecoration e & characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (AdditiveOptimizationDecoration a) Bool where

    isLeaf = lens getter setter
      where
         getter e   =     preorderDecoration e ^. isLeaf
         setter e x = e { preorderDecoration = preorderDecoration e & isLeaf .~ x }


-- | (✔)
instance (Bound a ~ c) => HasCharacterCost (AdditiveOptimizationDecoration a) c where

    characterCost = lens getter setter
      where
         getter e   =     preorderDecoration e ^. characterCost
         setter e x = e { preorderDecoration = preorderDecoration e & characterCost .~ x }


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (AdditiveOptimizationDecoration a) (Range c) where

    preliminaryInterval = lens getter setter
      where
         getter e   =     preorderDecoration e ^. preliminaryInterval
         setter e x = e { preorderDecoration = preorderDecoration e & preliminaryInterval .~ x }


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (AdditiveOptimizationDecoration a) (Range c, Range c) where

    childPrelimIntervals = lens getter setter
      where
         getter e   =     preorderDecoration e ^. childPrelimIntervals
         setter e x = e { preorderDecoration = preorderDecoration e & childPrelimIntervals .~ x }


-- | (✔)
instance (Bound a ~ c) => HasFinalInterval (AdditiveOptimizationDecoration a) (Range c) where

    finalInterval = lens additiveFinalInterval $ \e x -> e { additiveFinalInterval = x }


-- | (✔)
instance GeneralCharacterMetadata (AdditiveOptimizationDecoration a) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . preorderDecoration


-- | (✔)
instance DiscreteCharacterMetadata (AdditiveOptimizationDecoration a) where

    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . preorderDecoration


-- | (✔)
instance EncodableStaticCharacter a => DiscreteWithTcmCharacterMetadata (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance (Ranged c, Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (AdditiveOptimizationDecoration c) c => RangedPostorderDecoration (AdditiveOptimizationDecoration c) c where


{-
-- | (✔)
instance RangedExtensionPostorder (AdditiveOptimizationDecoration c) c where
-}


-- | (✔)
instance ( DiscreteCharacterMetadata    (AdditiveOptimizationDecoration a)
         , RangedDecorationOptimization (AdditiveOptimizationDecoration a) a
         , RangedPostorderDecoration    (AdditiveOptimizationDecoration a) a
         ) => RangedExtensionPreorder   (AdditiveOptimizationDecoration a) a where

    extendRangedToPreorder subDecoration finalInterval =

        AdditiveOptimizationDecoration
        { additiveFinalInterval = finalInterval
        , preorderDecoration    = preorder
        }
      where
        preorder =
            AdditivePreorderDecoration
            { additiveChildPrelimIntervals = subDecoration ^. childPrelimIntervals
            , additiveIsLeaf               = subDecoration ^. isLeaf
            , additiveCost                 = subDecoration ^. characterCost
            , additiveMetadataField        = extractDiscreteCharacterMetadata subDecoration
            , additivePreliminaryInterval  = subDecoration ^. preliminaryInterval
            , additiveCharacterField       = fromRange finalInterval
            }
