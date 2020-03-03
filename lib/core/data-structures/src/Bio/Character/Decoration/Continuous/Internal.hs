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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bio.Character.Decoration.Continuous.Internal
  ( ContinuousDecorationInitial(..)
  , ContinuousPostorderDecoration()
  , ContinuousOptimizationDecoration()
  , continuousDecorationInitial
  ) where

import Bio.Character.Decoration.Additive
import Bio.Character.Encodable
import Control.DeepSeq
import Control.Lens
import Data.Binary
import Data.Range
import GHC.Generics
import Numeric.Extended
import Text.XML
import TextShow                          (TextShow (showb), unlinesB)


-- |
-- An abstract initial continuous character decoration with a polymorphic character
-- type.
newtype ContinuousDecorationInitial c
    = ContinuousDecorationInitial
    { continuousDecorationInitialCharacter :: c
    }
    deriving stock   (Generic)
    deriving newtype (Ranged)


-- |
-- A smart constructor for a continuous character.
continuousDecorationInitial :: c -> ContinuousDecorationInitial c
continuousDecorationInitial value =
    ContinuousDecorationInitial
    { continuousDecorationInitialCharacter = value
    }


-- |
-- Represents a character decoration after a pre-order traversal.
newtype ContinuousOptimizationDecoration  c = COptD (AdditiveOptimizationDecoration c)
    deriving stock (Generic)


-- |
-- Represents the partially-complete character decoration after a post-order
-- traversal.
newtype ContinuousPostorderDecoration c = CPostD (AdditivePostorderDecoration c)
    deriving stock (Generic)


lensCOptD :: Functor f
          => Getting x (AdditiveOptimizationDecoration t) x
          -> ASetter (AdditiveOptimizationDecoration t) (AdditiveOptimizationDecoration c) a b
          -> (x -> f b)
          -> ContinuousOptimizationDecoration t
          -> f (ContinuousOptimizationDecoration c)
lensCOptD f g = lens (getterCPostD f) (setterCPostD g)
  where
    getterCPostD h (COptD e)   = e ^. h
    setterCPostD h (COptD e) x = COptD $ e & h .~ x


lensCPostD :: Functor f
           => Getting x (AdditivePostorderDecoration t) x
           -> ASetter (AdditivePostorderDecoration t) (AdditivePostorderDecoration c) a b
           -> (x -> f b)
           -> ContinuousPostorderDecoration t
           -> f (ContinuousPostorderDecoration c)
lensCPostD f g = lens (getterCPostD f) (setterCPostD g)
  where
    getterCPostD h (CPostD e)   = e ^. h
    setterCPostD h (CPostD e) x = CPostD $ e & h .~ x


type instance Bound (ContinuousDecorationInitial c) = Bound c


-- | (✔)
instance (Binary a, Binary (Finite (Bound a)), Binary (Range (Bound a))) => Binary (ContinuousOptimizationDecoration a)


-- | (✔)
instance (Binary a, Binary (Finite (Bound a)), Binary (Range (Bound a))) => Binary (ContinuousPostorderDecoration a)


-- | (✔)
instance (Finite (Bound a) ~ c) => HasCharacterCost (ContinuousOptimizationDecoration a) c where

    characterCost = lensCOptD characterCost characterCost


-- | (✔)
instance (Finite (Bound a) ~ c) => HasCharacterCost (ContinuousPostorderDecoration a) c where

    characterCost = lensCPostD characterCost characterCost


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (ContinuousOptimizationDecoration a) (Range c, Range c) where

    childPrelimIntervals = lensCOptD childPrelimIntervals childPrelimIntervals


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (ContinuousPostorderDecoration a) (Range c, Range c) where

    childPrelimIntervals = lensCPostD childPrelimIntervals childPrelimIntervals


{--}
-- WE LIE HERE FOR REASONS I DO NOT UNDERSTAND.
-- WE SHOULD USE ROW-POLYMORPHIC RECORDS.
instance HasDiscreteCharacter (ContinuousOptimizationDecoration a) a where

    discreteCharacter = lensCOptD discreteCharacter discreteCharacter
{--}


-- | (✔)
instance (Bound a ~ c) => HasFinalInterval (ContinuousOptimizationDecoration a) (Range c) where

    finalInterval = lensCOptD finalInterval finalInterval


-- | (✔)
instance HasIntervalCharacter (ContinuousDecorationInitial c) c where

    intervalCharacter = lens continuousDecorationInitialCharacter
                      $ \e x -> e { continuousDecorationInitialCharacter = x }


-- | (✔)
instance HasIntervalCharacter (ContinuousOptimizationDecoration a) a where

    intervalCharacter = lensCOptD intervalCharacter intervalCharacter


-- | (✔)
instance HasIntervalCharacter (ContinuousPostorderDecoration a) a where

    intervalCharacter = lensCPostD intervalCharacter intervalCharacter


-- | (✔)
instance HasIsLeaf (ContinuousOptimizationDecoration a) Bool where

    isLeaf = lensCOptD isLeaf isLeaf


-- | (✔)
instance HasIsLeaf (ContinuousPostorderDecoration a) Bool where

    isLeaf = lensCPostD isLeaf isLeaf


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (ContinuousOptimizationDecoration a) (Range c) where

    preliminaryInterval = lensCOptD preliminaryInterval preliminaryInterval


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (ContinuousPostorderDecoration a) (Range c) where

    preliminaryInterval = lensCPostD preliminaryInterval preliminaryInterval


instance NFData c => NFData (ContinuousDecorationInitial c)


instance (NFData c, NFData (Finite (Bound c)), NFData (Range (Bound c))) => NFData (ContinuousOptimizationDecoration c)


instance (NFData c, NFData (Finite (Bound c)), NFData (Range (Bound c))) => NFData (ContinuousPostorderDecoration c)


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (ContinuousDecorationInitial c) where

    isMissing = isMissing . (^. intervalCharacter)

    toMissing x = x & intervalCharacter %~ toMissing


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (ContinuousPostorderDecoration c) where

    isMissing = isMissing . (^. intervalCharacter)

    toMissing x = x & intervalCharacter %~ toMissing


-- | (✔)
instance ( Ranged c
         , ExtendedNumber (Bound c)
         , Num (Finite (Bound c))
         , Num (Bound c)
         , Ord (Bound c)
         ) => RangedCharacterDecoration (ContinuousDecorationInitial c) c where


-- | (✔)
instance ( Ranged c
         , ExtendedNumber (Bound c)
         , Num (Finite (Bound c))
         , Num (Bound c)
         , Ord (Bound c)
         ) => RangedCharacterDecoration (ContinuousOptimizationDecoration c) c where


-- | (✔)
instance ( RangedCharacterDecoration (ContinuousOptimizationDecoration c) c
         , HasFinalInterval (ContinuousOptimizationDecoration c) (Range (Bound c))
         ) => RangedDecorationOptimization (ContinuousOptimizationDecoration c) c where


-- | (✔)
instance ( Ranged c
         , ExtendedNumber (Bound c)
         , Num (Finite (Bound c))
         , Num (Bound c)
         , Ord (Bound c)
         ) => RangedCharacterDecoration (ContinuousPostorderDecoration c) c where


-- | (✔)
instance ( -- DiscreteCharacterMetadata   (ContinuousPostorderDecoration a)
           RangedPostorderDecoration   (ContinuousPostorderDecoration a) a
         , RangedCharacterDecoration   (AdditivePostorderDecoration a) a
         , RangedPostorderDecoration   (AdditivePostorderDecoration a) a
         ) => RangedExtensionPostorder (ContinuousPostorderDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =

        CPostD $ extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal


-- | (✔)
instance ( -- DiscreteCharacterMetadata    (ContinuousOptimizationDecoration a)
           RangedDecorationOptimization (ContinuousOptimizationDecoration a) a
         , RangedCharacterDecoration    (AdditiveOptimizationDecoration   a) a
         , RangedExtensionPreorder      (AdditiveOptimizationDecoration   a) a
         ) => RangedExtensionPreorder   (ContinuousOptimizationDecoration a) a where

    extendRangedToPreorder subDecoration intervalValue =

        COptD $ extendRangedToPreorder subDecoration intervalValue


-- | (✔)
instance RangedCharacterDecoration (ContinuousOptimizationDecoration c) c => RangedPostorderDecoration (ContinuousOptimizationDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (ContinuousPostorderDecoration c) c => RangedPostorderDecoration (ContinuousPostorderDecoration c) c where


-- | (✔)
instance Show c => Show (ContinuousDecorationInitial c) where

    show = show . (^. intervalCharacter)

-- | (✔)
instance TextShow c => TextShow (ContinuousDecorationInitial c) where

    showb = showb . (^. intervalCharacter)


-- | (✔)
instance
    ( Show c
    , Show (Bound c)
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => Show (ContinuousOptimizationDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Continuous Character : " <> show (c ^. intervalCharacter)
        , "Preliminary Interval : " <> show (c ^. preliminaryInterval)
        , "Child       Intervals: " <> show (c ^. childPrelimIntervals)
        , "Final       Interval : " <> show (c ^. finalInterval)
        ]

-- | (✔)
instance
    ( TextShow c
    , TextShow (Bound c)
    , TextShow (Finite (Bound c))
    , TextShow (Range  (Bound c))
    ) => TextShow (ContinuousOptimizationDecoration c) where

    showb c = unlinesB
        [ "Cost = "                 <> showb (c ^. characterCost)
        , "Is Leaf Node?        : " <> showb (c ^. isLeaf)
        , "Continuous Character : " <> showb (c ^. intervalCharacter)
        , "Preliminary Interval : " <> showb (c ^. preliminaryInterval)
        , "Child       Intervals: " <> showb (c ^. childPrelimIntervals)
        , "Final       Interval : " <> showb (c ^. finalInterval)
        ]


-- | (✔)
instance
    ( Show c
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => Show (ContinuousPostorderDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Continuous Character : " <> show (c ^. intervalCharacter)
        , "Preliminary Interval : " <> show (c ^. preliminaryInterval)
        , "Child       Intervals: " <> show (c ^. childPrelimIntervals)
        ]

-- | (✔)
instance
    ( TextShow c
    , TextShow (Finite (Bound c))
    , TextShow (Range  (Bound c))
    ) => TextShow (ContinuousPostorderDecoration c) where

    showb c = unlinesB
        [ "Cost = "                 <> showb (c ^. characterCost)
        , "Is Leaf Node?        : " <> showb (c ^. isLeaf)
        , "Continuous Character : " <> showb (c ^. intervalCharacter)
        , "Preliminary Interval : " <> showb (c ^. preliminaryInterval)
        , "Child       Intervals: " <> showb (c ^. childPrelimIntervals)
        ]


-- | Create xml instance for initial decoration, which is empty.
instance (Show c) => ToXML (ContinuousDecorationInitial c) where
    toXML (ContinuousDecorationInitial val {-metadata-}) = xmlElement "ContinuousDecorationInitial" attributes contents
        where
            attributes = []
            contents   = [ Left ("Initial_character_state", show val) ]


-- | Create xml instance for preorder decoration, which has a finalized state interval.
instance
    ( Show c
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (ContinuousOptimizationDecoration c) where

    toXML optimizationDecoration = xmlElement "ContinuousOptimizationDecoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Cost"                , show $ optimizationDecoration ^. characterCost       )
                         , Left ("Is_leaf_node"        , show $ optimizationDecoration ^. isLeaf              )
                         , Left ("Continuous_character", show $ optimizationDecoration ^. intervalCharacter   )
                         , Left ("Preliminary_interval", show $ optimizationDecoration ^. preliminaryInterval )
                         , Left ("Child_intervals"     , show $ optimizationDecoration ^. childPrelimIntervals)
                         , Left ("Final_interval"      , show $ optimizationDecoration ^. finalInterval       )
                         ]


-- | Create xml instance for postorder decoration, which has no final state interval set.
instance
    ( Show c
    , Show (Finite (Bound c))
    , Show (Range  (Bound c))
    ) => ToXML (ContinuousPostorderDecoration c) where

    toXML postorderDecoration = xmlElement "ContinuousPostorderDecoration" attributes contents
        where
            attributes = []
            contents   = [ Left ("Cost"                , show $ postorderDecoration ^. characterCost       )
                         , Left ("Is_leaf_node?"       , show $ postorderDecoration ^. isLeaf              )
                         , Left ("Continuous_character", show $ postorderDecoration ^. intervalCharacter   )
                         , Left ("Preliminary_interval", show $ postorderDecoration ^. preliminaryInterval )
                         , Left ("Child_intervals"     , show $ postorderDecoration ^. childPrelimIntervals)
                         ]
