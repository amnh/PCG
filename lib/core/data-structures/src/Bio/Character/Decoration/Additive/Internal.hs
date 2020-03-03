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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- We need this for the generalized type family derivation of Ranged instances.
{-# LANGUAGE UndecidableInstances  #-}

module Bio.Character.Decoration.Additive.Internal where

import Bio.Character.Decoration.Additive.Class
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Control.DeepSeq
import Control.Lens
import Data.Binary
--import Data.Foldable
import Data.Range
import GHC.Generics
import Numeric.Extended
import Text.XML
import TextShow                                (TextShow (showb), unlinesB)


-- |
-- Represents the finalized character decoration after a pre-order traversal.
data  AdditiveOptimizationDecoration a
    = AdditiveOptimizationDecoration
    { additiveFinalInterval :: {-# UNPACK #-} !(Range (Bound a))
    , postorderDecoration   :: {-# UNPACK #-} !(AdditivePostorderDecoration a)
    }
    deriving stock    (Generic)


-- |
-- An abstract initial dynamic character decoration with a polymorphic character
-- type.
data  AdditivePostorderDecoration a
    = AdditivePostorderDecoration
    { additiveCost                 :: !(Finite (Bound a))
    , additivePreliminaryInterval  :: {-# UNPACK #-} !(Range (Bound a))
    , additiveChildPrelimIntervals :: {-# UNPACK #-} !(Range (Bound a), Range (Bound a))
    , additiveIsLeaf               :: !Bool
    , additiveCharacterField       :: !a
    }
    deriving stock    (Generic)


{-
-- | (✔)
instance ( Binary a
         , Binary (Bound a)
         , Binary (Finite (Bound a))
         ) => Binary (AdditivePostorderDecoration a) where

    put x = fold $
              [ put . additiveCost
              , put . additivePreliminaryInterval
              , put . additiveChildPrelimIntervals
              , put . additiveIsLeaf
              , put . additiveCharacterField
              ] <*> [x]

    get = AdditivePostorderDecoration <$> get <*> get <*> get <*> get <*> get
-}


-- | (✔)
instance (Binary a, Binary (Finite (Bound a)), Binary (Range (Bound a))) => Binary (AdditiveOptimizationDecoration a)


-- | (✔)
instance (Binary a, Binary (Finite (Bound a)), Binary (Range (Bound a))) => Binary (AdditivePostorderDecoration a)


-- | (✔)
instance HasIntervalCharacter (AdditivePostorderDecoration a) a where

    intervalCharacter = discreteCharacter


-- | (✔)
instance HasDiscreteCharacter (AdditivePostorderDecoration a) a where

    discreteCharacter = lens additiveCharacterField (\e x -> e { additiveCharacterField = x })


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
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditivePostorderDecoration a) a where


-- | (✔)
instance (NFData a, NFData (Finite (Bound a)), NFData (Range (Bound a))) => NFData (AdditiveOptimizationDecoration a)


-- | (✔)
instance (NFData a, NFData (Finite (Bound a)), NFData (Range (Bound a))) => NFData (AdditivePostorderDecoration a)


-- | (✔)
instance (Ranged c, ExtendedNumber (Bound c), Num (Finite (Bound c)), Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (AdditivePostorderDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (AdditivePostorderDecoration c) c => RangedPostorderDecoration (AdditivePostorderDecoration c) c where


-- | (✔)
instance RangedPostorderDecoration (AdditivePostorderDecoration a) a => RangedExtensionPostorder (AdditivePostorderDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =

        AdditivePostorderDecoration
        { additiveChildPrelimIntervals = childMedianTup
        , additiveIsLeaf               = isLeafVal
        , additiveCost                 = cost
        , additivePreliminaryInterval  = prelimInterval
        , additiveCharacterField       = subDecoration ^. intervalCharacter
        }


-- | (✔)
instance
  ( EncodableStreamElement c
  , Show c
  , Show (Bound c)
  , Show (Finite (Bound c))
  , Show (Range  (Bound c))
  ) => Show (AdditiveOptimizationDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost       )
        , "Is Leaf Node?        : " <> show (c ^. isLeaf              )
        , "Discrete Character   : " <> show (c ^. discreteCharacter   )
        , "Preliminary Interval : " <> show (c ^. preliminaryInterval )
        , "Child       Intervals: " <> show (c ^. childPrelimIntervals)
        , "Final       Interval : " <> show (c ^. finalInterval       )
        ]

-- | (✔)
instance
  ( EncodableStreamElement c
  , TextShow c
  , TextShow (Bound c)
  , TextShow (Finite (Bound c))
  , TextShow (Range  (Bound c))
  ) => TextShow (AdditiveOptimizationDecoration c) where

    showb c = unlinesB
        [ "Cost = "                 <> showb (c ^. characterCost       )
        , "Is Leaf Node?        : " <> showb (c ^. isLeaf              )
        , "Discrete Character   : " <> showb (c ^. discreteCharacter   )
        , "Preliminary Interval : " <> showb (c ^. preliminaryInterval )
        , "Child       Intervals: " <> showb (c ^. childPrelimIntervals)
        , "Final       Interval : " <> showb (c ^. finalInterval       )
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
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (AdditiveOptimizationDecoration a) a where


-- | (✔)
instance (Ranged c, ExtendedNumber (Bound c), Num (Finite (Bound c)), Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (AdditiveOptimizationDecoration c) c => RangedPostorderDecoration (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance ( RangedCharacterDecoration (AdditiveOptimizationDecoration c) c
         , HasFinalInterval (AdditiveOptimizationDecoration c) (Range (Bound c))
         ) => RangedDecorationOptimization (AdditiveOptimizationDecoration c) c where


-- | (✔)
instance ( RangedDecorationOptimization (AdditiveOptimizationDecoration a) a
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
            , additivePreliminaryInterval  = subDecoration ^. preliminaryInterval
            , additiveCharacterField       = fromRange interval
            }


-- | (✔)
instance
  ( EncodableStreamElement c
  , Show c
  , Show (Bound c)
  , Show (Finite (Bound c))
  , Show (Range  (Bound c))
  ) => Show (AdditivePostorderDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Discrete Character   : " <> show (c ^. discreteCharacter)
        , "Preliminary Interval : " <> show (additivePreliminaryInterval c)
        , "Child       Intervals: " <> show (additiveChildPrelimIntervals c)
        ]

-- | (✔)
instance
  ( EncodableStreamElement c
  , TextShow c
  , TextShow (Bound c)
  , TextShow (Finite (Bound c))
  , TextShow (Range  (Bound c))
  ) => TextShow (AdditivePostorderDecoration c) where

    showb c = unlinesB
        [ "Cost = "                 <> showb (c ^. characterCost)
        , "Is Leaf Node?        : " <> showb (c ^. isLeaf)
        , "Discrete Character   : " <> showb (c ^. discreteCharacter)
        , "Preliminary Interval : " <> showb (additivePreliminaryInterval c)
        , "Child       Intervals: " <> showb (additiveChildPrelimIntervals c)
        ]


-- | (✔)
instance
    ( Show c
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (AdditivePostorderDecoration c) where

    toXML decoration = xmlElement "Additive postorder decoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Cost"                 , show $ decoration ^. characterCost             )
                         , Left ("Is leaf"              , show $ decoration ^. isLeaf                    )
                         , Left ("Discrete Character"   , show $ decoration ^. discreteCharacter         )
                         , Left ("Preliminary Interval" , show $ additivePreliminaryInterval  decoration )
                         , Left ("Child Intervals:"     , show $ additiveChildPrelimIntervals decoration )
                         ]


-- | (✔)
instance
    ( Show c
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (AdditiveOptimizationDecoration c) where

    toXML decoration = xmlElement "Additive operation decoration" attributes contents
      where
        attributes = []
        contents   = [ Left ("Final interval"       , show $ decoration ^. finalInterval       )
                     , Left ("Cost"                 , show $ decoration ^. characterCost       )
                     , Left ("Is leaf"              , show $ decoration ^. isLeaf              )
                     , Left ("Discrete Character"   , show $ decoration ^. discreteCharacter   )
                     , Left ("Preliminary Interval" , show $ decoration ^. preliminaryInterval )
                     , Left ("Child Intervals:"     , show $ decoration ^. childPrelimIntervals)
                     ]
