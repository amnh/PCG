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
import Numeric.Extended
import Text.XML.Custom
import Text.XML.Light


-- |
-- Represents the finalized character decoration after a pre-order traversal.
data AdditiveOptimizationDecoration a
   = AdditiveOptimizationDecoration
   { additiveFinalInterval :: Range (Bound a)
   , postorderDecoration   :: AdditivePostorderDecoration a
   }


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data AdditivePostorderDecoration a
   = AdditivePostorderDecoration
   { additiveCost                 :: Finite (Bound a)
   , additivePreliminaryInterval  :: Range (Bound a)
   , additiveChildPrelimIntervals :: (Range (Bound a), Range (Bound a))
   , additiveIsLeaf               :: Bool
   , additiveCharacterField       :: a
   , additiveMetadataField        :: DiscreteCharacterMetadataDec
   }


-- | (✔)
instance HasIntervalCharacter (AdditivePostorderDecoration a) a where

    intervalCharacter = discreteCharacter


-- | (✔)
instance HasDiscreteCharacter (AdditivePostorderDecoration a) a where

    discreteCharacter = lens additiveCharacterField (\e x -> e { additiveCharacterField = x })


-- | (✔)
instance HasCharacterAlphabet (AdditivePostorderDecoration a) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterAlphabet
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditivePostorderDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterName
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterName .~ x }


-- | (✔)
instance HasSymbolChangeMatrix (AdditivePostorderDecoration a) (Word -> Word -> Word) where

    symbolChangeMatrix = lens getter setter
      where
        getter = const $ \i j -> max i j - min i j
        setter = const


-- | (✔)
instance HasTransitionCostMatrix (AdditivePostorderDecoration a) (a -> a -> (a, Word)) where


    -- NOTE: This probably isn't sound
    transitionCostMatrix = lens getter setter
      where
        getter = error "Please don't use lens accessor operations over 'transitionCostMatrix' on a AdditiveOptimizationDecoration."
        setter = const


-- | (✔)
instance HasCharacterWeight (AdditivePostorderDecoration a) Double where

    characterWeight = lens getter setter
      where
         getter e   = additiveMetadataField e ^. characterWeight
         setter e x = e { additiveMetadataField = additiveMetadataField e &  characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (AdditivePostorderDecoration a) Bool where

    isLeaf = lens additiveIsLeaf (\e x -> e { additiveIsLeaf = x })


-- | (✔)
instance (Finite (Bound a) ~ c) => HasCharacterCost (AdditivePostorderDecoration a) c where

    characterCost = lens additiveCost (\e x -> e { additiveCost = x })


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (AdditivePostorderDecoration a) (Range c) where

    preliminaryInterval = lens additivePreliminaryInterval (\e x -> e { additivePreliminaryInterval = x })


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (AdditivePostorderDecoration a) (Range c, Range c) where

    childPrelimIntervals = lens additiveChildPrelimIntervals (\e x -> e { additiveChildPrelimIntervals = x })


-- | (✔)
instance GeneralCharacterMetadata (AdditivePostorderDecoration a) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . additiveMetadataField


-- | (✔)
instance DiscreteCharacterMetadata (AdditivePostorderDecoration a) where

    extractDiscreteCharacterMetadata = additiveMetadataField


-- | (✔)
instance EncodableStaticCharacter a => DiscreteWithTcmCharacterMetadata (AdditivePostorderDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditivePostorderDecoration a) a where


-- | (✔)
instance (Ranged c, ExtendedNumber (Bound c), Num (Finite (Bound c)), Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (AdditivePostorderDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (AdditivePostorderDecoration c) c => RangedPostorderDecoration (AdditivePostorderDecoration c) c where

  {-
-- | (✔)
instance RangedCharacterDecoration s c
         , HasCharacterCost s (Bound c)
         , HasChildPrelimIntervals s (Range (Bound c), Range (Bound c))
         , HasIsLeaf s Bool
         , HasPreliminaryInterval s (Range (Bound c))
         ) => RangedPostorderDecoration (AdditivePostorderDecoration a) a where
-}

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
instance ( DiscreteCharacterMetadata   (AdditivePostorderDecoration a)
         , RangedPostorderDecoration   (AdditivePostorderDecoration a) a
         ) => RangedExtensionPostorder (AdditivePostorderDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =

        AdditivePostorderDecoration
        { additiveChildPrelimIntervals = childMedianTup
        , additiveIsLeaf               = isLeafVal
        , additiveCost                 = cost
        , additiveMetadataField        = extractDiscreteCharacterMetadata subDecoration
        , additivePreliminaryInterval  = prelimInterval
        , additiveCharacterField       = subDecoration ^. intervalCharacter
        }





-- | (✔)
instance
  ( EncodableStreamElement c
  , Show (Bound c)
  , Show (Finite (Bound c))
  , Show (Range  (Bound c))
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
instance HasIntervalCharacter (AdditiveOptimizationDecoration a) a where

    {-# INLINE intervalCharacter #-}
    intervalCharacter = discreteCharacter


-- | (✔)
instance HasDiscreteCharacter (AdditiveOptimizationDecoration a) a where

    {-# INLINE discreteCharacter #-}
    discreteCharacter = lens getter setter
      where
        getter e   =     postorderDecoration e ^. discreteCharacter
        setter e x = e { postorderDecoration = postorderDecoration e & discreteCharacter .~ x }


-- | (✔)
instance HasCharacterAlphabet (AdditiveOptimizationDecoration a) (Alphabet String) where

    {-# INLINE characterAlphabet #-}
    characterAlphabet = lens getter setter
      where
         getter e   =     postorderDecoration e ^. characterAlphabet
         setter e x = e { postorderDecoration = postorderDecoration e & characterAlphabet .~ x }


-- | (✔)
instance HasCharacterName (AdditiveOptimizationDecoration a) CharacterName where

    characterName = lens getter setter
      where
         getter e   =     postorderDecoration e ^. characterName
         setter e x = e { postorderDecoration = postorderDecoration e & characterName .~ x }


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
         getter e   =     postorderDecoration e ^. characterWeight
         setter e x = e { postorderDecoration = postorderDecoration e & characterWeight .~ x }


-- | (✔)
instance HasIsLeaf (AdditiveOptimizationDecoration a) Bool where

    isLeaf = lens getter setter
      where
         getter e   =     postorderDecoration e ^. isLeaf
         setter e x = e { postorderDecoration = postorderDecoration e & isLeaf .~ x }


-- | (✔)
instance (Finite (Bound a) ~ c) => HasCharacterCost (AdditiveOptimizationDecoration a) c where

    characterCost = lens getter setter
      where
         getter e   =     postorderDecoration e ^. characterCost
         setter e x = e { postorderDecoration = postorderDecoration e & characterCost .~ x }


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (AdditiveOptimizationDecoration a) (Range c) where

    preliminaryInterval = lens getter setter
      where
         getter e   =     postorderDecoration e ^. preliminaryInterval
         setter e x = e { postorderDecoration = postorderDecoration e & preliminaryInterval .~ x }


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (AdditiveOptimizationDecoration a) (Range c, Range c) where

    childPrelimIntervals = lens getter setter
      where
         getter e   =     postorderDecoration e ^. childPrelimIntervals
         setter e x = e { postorderDecoration = postorderDecoration e & childPrelimIntervals .~ x }


-- | (✔)
instance (Bound a ~ c) => HasFinalInterval (AdditiveOptimizationDecoration a) (Range c) where

    finalInterval = lens additiveFinalInterval $ \e x -> e { additiveFinalInterval = x }


-- | (✔)
instance GeneralCharacterMetadata (AdditiveOptimizationDecoration a) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . postorderDecoration


-- | (✔)
instance DiscreteCharacterMetadata (AdditiveOptimizationDecoration a) where

    extractDiscreteCharacterMetadata = extractDiscreteCharacterMetadata . postorderDecoration


-- | (✔)
instance EncodableStaticCharacter a => DiscreteWithTcmCharacterMetadata (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance (Ranged c, ExtendedNumber (Bound c), Num (Finite (Bound c)), Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (AdditiveOptimizationDecoration c) c => RangedPostorderDecoration (AdditiveOptimizationDecoration c) c where


--n| (✔)
--instance RangedCharacterDecoration (AdditiveOptimizationDecoration c) c => RangedExtensionPostorder (AdditiveOptimizationDecoration c) c where

-- | (✔)
instance ( RangedCharacterDecoration (AdditiveOptimizationDecoration c) c
         , HasFinalInterval (AdditiveOptimizationDecoration c) (Range (Bound c))
         ) => RangedDecorationOptimization (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance ( DiscreteCharacterMetadata    (AdditiveOptimizationDecoration a)
         , RangedDecorationOptimization (AdditiveOptimizationDecoration a) a
         , RangedPostorderDecoration    (AdditiveOptimizationDecoration a) a
         ) => RangedExtensionPreorder   (AdditiveOptimizationDecoration a) a where

    extendRangedToPreorder subDecoration interval =

        AdditiveOptimizationDecoration
        { additiveFinalInterval = interval
        , postorderDecoration    = preorder
        }
      where
        preorder =
            AdditivePostorderDecoration
            { additiveChildPrelimIntervals = subDecoration ^. childPrelimIntervals
            , additiveIsLeaf               = subDecoration ^. isLeaf
            , additiveCost                 = subDecoration ^. characterCost
            , additiveMetadataField        = extractDiscreteCharacterMetadata subDecoration
            , additivePreliminaryInterval  = subDecoration ^. preliminaryInterval
            , additiveCharacterField       = fromRange interval
            }


{--
-- | (✔)
instance ( RangedPostorderDecoration   (AdditiveOptimizationDecoration a) a
         ) => RangedExtensionPostorder (AdditiveOptimizationDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =
        subDecoration { postorderDecoration = extendRangedToPostorder (postorderDecoration subDecoration) cost prelimInterval childMedianTup isLeafVal }
--}


-- | (✔)
instance
  ( EncodableStreamElement c
  , Show (Bound c)
  , Show (Finite (Bound c))
  , Show (Range  (Bound c))
  ) => Show (AdditivePostorderDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Discrete Character   : " <> showDiscreteCharacterElement c
        , "Preliminary Interval : " <> show (additivePreliminaryInterval c)
        , "Child       Intervals: " <> show (additiveChildPrelimIntervals c)
        ]


-- | (✔)
instance
    ( EncodableStreamElement c
    -- , Show (Bound c)
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (AdditivePostorderDecoration c) where

    toXML decoration = xmlElement "Additive postorder decoration" attributes contents
        where
            attributes = []
            contents   = [ ("Cost"                 , show $ decoration ^. characterCost             )
                         , ("Is leaf"              , show $ decoration ^. isLeaf                    )
                         , ("Discrete Character"   , showDiscreteCharacterElement        decoration )
                         , ("Preliminary Interval" , show $ additivePreliminaryInterval  decoration )
                         , ("Child Intervals:"     , show $ additiveChildPrelimIntervals decoration )
                         ]


-- | (✔)
instance
    ( EncodableStreamElement c
    -- , Show (Bound c)
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (AdditiveOptimizationDecoration c) where

    toXML decoration = xmlElement "Additive operation decoration" attributes contents
        where
            attributes = []
            contents   = [ ("Final interval" , Left  . show $ decoration ^. finalInterval)
                         , ("Decoration"     , Right . toXML decoration                  )
                         ]
