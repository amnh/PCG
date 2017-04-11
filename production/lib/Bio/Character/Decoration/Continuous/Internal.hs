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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Bio.Character.Decoration.Continuous.Internal where


import Bio.Character.Decoration.Additive
--import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Continuous
import Bio.Metadata.Discrete
import Control.Lens
import Data.Alphabet
import Data.Range
import Data.Semigroup


-- |
-- An abstract initial continuous character decoration with a polymorphic character
-- type.
data ContinuousDecorationInitial c
   = ContinuousDecorationInitial
   { continuousDecorationInitialCharacter :: c
   , continuousMetadataField              :: ContinuousCharacterMetadataDec
   }


-- |
-- A smart constructor for a continuous character.
continuousDecorationInitial :: CharacterName -> Double -> c -> ContinuousDecorationInitial c
continuousDecorationInitial name weight value =
    ContinuousDecorationInitial
    { continuousDecorationInitialCharacter = value
    , continuousMetadataField              = continuousMetadata name weight
    }


-- | (✔)
instance PossiblyMissingCharacter c => PossiblyMissingCharacter (ContinuousDecorationInitial c) where

    isMissing = isMissing . (^. intervalCharacter)

    toMissing x = x & intervalCharacter %~ toMissing


{-
instance HasDiscreteCharacter (ContinuousDecorationInitial a) a where

    discreteCharacter = lens continuousDecorationInitialCharacter
                      $ \e x -> e { continuousDecorationInitialCharacter = x }
-}


-- | (✔)
instance HasIntervalCharacter (ContinuousDecorationInitial c) c where

    intervalCharacter = lens continuousDecorationInitialCharacter
                      $ \e x -> e { continuousDecorationInitialCharacter = x }


-- | (✔)
instance Show c => Show (ContinuousDecorationInitial c) where

    show = show . (^. intervalCharacter)


{-
-- | (✔)
instance ContinuousCharacter ContinuousChar where

    toContinuousCharacter = CC . fmap (fromRational . toRational)

-}

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


-- | This will throw errors, I advise against using this Lens.
instance HasCharacterAlphabet (ContinuousDecorationInitial c) (Alphabet String) where

    characterAlphabet = lens getter setter
      where
         setter e _ = e
         getter _   = error $ unwords
            [ "This is a kluge."
            , "Please don't ask for the alphabet of a continuous character."
            , "This exists solely to satisfy incoherent constraints due to bad design desicions."
            , "My bad team :("
            ]


{-
-- | (✔)
instance HasContinuousCharacter (ContinuousDecorationInitial c) c where

    continuousCharacter = lens continuousDecorationInitialCharacter $ \e x -> e { continuousDecorationInitialCharacter = x }
-}


-- | (✔)
instance GeneralCharacterMetadata (ContinuousDecorationInitial d) where

    extractGeneralCharacterMetadata = extractGeneralCharacterMetadata . continuousMetadataField


-- | (✔)
instance DiscreteCharacterMetadata (ContinuousDecorationInitial d) where

    extractDiscreteCharacterMetadata x = discreteMetadata
                                           (x ^. characterName    )
                                           (x ^. characterWeight  )
                                           (x ^. characterAlphabet)

-- | (✔)
instance (Ranged c, Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (ContinuousDecorationInitial c) c where


{-
-- | (✔)
instance ContinuousCharacter c => ContinuousDecoration (ContinuousDecorationInitial c) c where
-}

  

{-
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
-}


newtype ContinuousPostorderDecoration c = CPostD (AdditivePostorderDecoration c)


lensCPostD :: Functor f
           => Getting a1 (AdditivePostorderDecoration t) a1
           -> ASetter (AdditivePostorderDecoration t) (AdditivePostorderDecoration c) a b
           -> (a1 -> f b)
           -> ContinuousPostorderDecoration t
           -> f (ContinuousPostorderDecoration c)
lensCPostD f g = lens (getterCPostD f) (setterCPostD g)
  where
    getterCPostD h (CPostD e)   = e ^. h
    setterCPostD h (CPostD e) x = CPostD $ e & h .~ x


instance
    ( Show c
    , Show (Bound c)
    , Show (Range (Bound c))
    ) => Show (ContinuousPostorderDecoration c) where

    show c = unlines
        [ "Cost = "                 <> show (c ^. characterCost)
        , "Is Leaf Node?        : " <> show (c ^. isLeaf)
        , "Continuous Character : " <> show (c ^. intervalCharacter)
        , "Preliminary Interval : " <> show (c ^. preliminaryInterval)
        , "Child       Intervals: " <> show (c ^. childPrelimIntervals)
        ]


-- | (✔)
instance HasIntervalCharacter (ContinuousPostorderDecoration a) a where

    intervalCharacter = lensCPostD intervalCharacter intervalCharacter


-- | (✔)
instance HasCharacterAlphabet (ContinuousPostorderDecoration a) (Alphabet String) where

    characterAlphabet = lensCPostD characterAlphabet characterAlphabet


-- | (✔)
instance HasCharacterName (ContinuousPostorderDecoration a) CharacterName where

    characterName = lensCPostD characterName characterName


-- | (✔)
instance HasCharacterWeight (ContinuousPostorderDecoration a) Double where

    characterWeight = lensCPostD characterWeight characterWeight


-- | (✔)
instance HasIsLeaf (ContinuousPostorderDecoration a) Bool where

    isLeaf = lensCPostD isLeaf isLeaf


-- | (✔)
instance (Bound a ~ c) => HasCharacterCost (ContinuousPostorderDecoration a) c where

    characterCost = lensCPostD characterCost characterCost


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (ContinuousPostorderDecoration a) (Range c) where

    preliminaryInterval = lensCPostD preliminaryInterval preliminaryInterval


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (ContinuousPostorderDecoration a) (Range c, Range c) where

    childPrelimIntervals = lensCPostD childPrelimIntervals childPrelimIntervals


-- | (✔)
instance GeneralCharacterMetadata (ContinuousPostorderDecoration a) where

    extractGeneralCharacterMetadata (CPostD x) = extractGeneralCharacterMetadata x


-- | (✔)
instance DiscreteCharacterMetadata (ContinuousPostorderDecoration a) where

    extractDiscreteCharacterMetadata (CPostD x) = extractDiscreteCharacterMetadata x


-- | (✔)
instance (Ranged c, Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (ContinuousPostorderDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (ContinuousPostorderDecoration c) c => RangedPostorderDecoration (ContinuousPostorderDecoration c) c where


{-
-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (ContinuousPostorderDecoration a) a where
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
instance ( DiscreteCharacterMetadata   (ContinuousPostorderDecoration a)
         , RangedPostorderDecoration   (ContinuousPostorderDecoration a) a
         , RangedCharacterDecoration   (AdditivePostorderDecoration a) a
         , RangedPostorderDecoration   (AdditivePostorderDecoration a) a
         ) => RangedExtensionPostorder (ContinuousPostorderDecoration a) a where

    extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal =

        CPostD $ extendRangedToPostorder subDecoration cost prelimInterval childMedianTup isLeafVal






newtype ContinuousOptimizationDecoration  c = COptD  (AdditiveOptimizationDecoration c)


lensCOptD :: Functor f
          => Getting a1 (AdditiveOptimizationDecoration t) a1
          -> ASetter (AdditiveOptimizationDecoration t) (AdditiveOptimizationDecoration c) a b
          -> (a1 -> f b)
          -> ContinuousOptimizationDecoration t
          -> f (ContinuousOptimizationDecoration c)
lensCOptD f g = lens (getterCPostD f) (setterCPostD g)
  where
    getterCPostD h (COptD e)   = e ^. h
    setterCPostD h (COptD e) x = COptD $ e & h .~ x


instance
    ( Show c
    , Show (Bound c)
    , Show (Range (Bound c))
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
instance HasIntervalCharacter (ContinuousOptimizationDecoration a) a where

    intervalCharacter = lensCOptD intervalCharacter intervalCharacter


-- | (✔)
instance HasCharacterAlphabet (ContinuousOptimizationDecoration a) (Alphabet String) where

    characterAlphabet = lensCOptD characterAlphabet characterAlphabet


-- | (✔)
instance HasCharacterName (ContinuousOptimizationDecoration a) CharacterName where

    characterName = lensCOptD characterName characterName


-- | (✔)
instance HasCharacterWeight (ContinuousOptimizationDecoration a) Double where

    characterWeight = lensCOptD characterWeight characterWeight


-- | (✔)
instance HasIsLeaf (ContinuousOptimizationDecoration a) Bool where

    isLeaf = lensCOptD isLeaf isLeaf


-- | (✔)
instance (Bound a ~ c) => HasCharacterCost (ContinuousOptimizationDecoration a) c where

    characterCost = lensCOptD characterCost characterCost


-- | (✔)
instance (Bound a ~ c) => HasPreliminaryInterval (ContinuousOptimizationDecoration a) (Range c) where

    preliminaryInterval = lensCOptD preliminaryInterval preliminaryInterval


-- | (✔)
instance (Bound a ~ c) => HasChildPrelimIntervals (ContinuousOptimizationDecoration a) (Range c, Range c) where

    childPrelimIntervals = lensCOptD childPrelimIntervals childPrelimIntervals


-- | (✔)
instance (Bound a ~ c) => HasFinalInterval (ContinuousOptimizationDecoration a) (Range c) where

    finalInterval = lensCOptD finalInterval finalInterval


-- | (✔)
instance GeneralCharacterMetadata (ContinuousOptimizationDecoration a) where

    extractGeneralCharacterMetadata (COptD x) = extractGeneralCharacterMetadata x


-- | (✔)
instance DiscreteCharacterMetadata (ContinuousOptimizationDecoration a) where

    extractDiscreteCharacterMetadata (COptD x) = extractDiscreteCharacterMetadata x


-- | (✔)
instance (Ranged c, Num (Bound c), Ord (Bound c)) => RangedCharacterDecoration (ContinuousOptimizationDecoration c) c where


-- | (✔)
instance RangedCharacterDecoration (ContinuousOptimizationDecoration c) c => RangedPostorderDecoration (ContinuousOptimizationDecoration c) c where


-- | (✔)
instance ( RangedCharacterDecoration (ContinuousOptimizationDecoration c) c
         , HasFinalInterval (ContinuousOptimizationDecoration c) (Range (Bound c))
         ) => RangedDecorationOptimization (ContinuousOptimizationDecoration c) c where 

{-
-- | (✔)
instance EncodableStaticCharacter a => DiscreteCharacterDecoration (ContinuousOptimizationDecoration a) a where
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
instance ( DiscreteCharacterMetadata    (ContinuousOptimizationDecoration a)
         , RangedDecorationOptimization (ContinuousOptimizationDecoration a) a
         , RangedCharacterDecoration    (AdditiveOptimizationDecoration   a) a
         , RangedExtensionPreorder      (AdditiveOptimizationDecoration   a) a
         ) => RangedExtensionPreorder   (ContinuousOptimizationDecoration a) a where

    extendRangedToPreorder subDecoration intervalValue =

        COptD $ extendRangedToPreorder subDecoration intervalValue


