------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.Sequence.Block
  ( CharacterBlock(..)
  , toMissingCharacters
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  , hexliftA2
  , hexmap
  , hexsequence
  ) where


import Bio.Character.Encodable
import Bio.Character.Decoration.Continuous
import Bio.Metadata.CharacterName
import Control.Applicative                 (liftA2)
import Data.Foldable
import Data.Monoid                         (mappend)
import Data.Semigroup
--import Data.Semigroup.Traversable
import Data.TCM
import Data.Vector                         (Vector)
import qualified Data.Vector as V


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharacterBlock' is polymorphic over static and dynamic character
-- definitions.
--
-- Use '(<>)' to construct larger blocks.
data CharacterBlock m i c f a d
   = CharacterBlock
   { continuousCharacterBins  :: Vector c
   , nonAdditiveCharacterBins :: Vector f
   , additiveCharacterBins    :: Vector a
   , metricCharacterBins      :: Vector m
   , nonMetricCharacterBins   :: Vector i
   , dynamicCharacters        :: Vector d
   } deriving (Eq)


-- |
-- Perform a six way map over the polymorphic types.
hexmap :: (m -> m')
       -> (i -> i')
       -> (c -> c')
       -> (f -> f')
       -> (a -> a')
       -> (d -> d')
       -> CharacterBlock m  i  c  f  a  d
       -> CharacterBlock m' i' c' f' a' d'
hexmap f1 f2 f3 f4 f5 f6 =
    CharacterBlock
      <$> (fmap f3 . continuousCharacterBins )
      <*> (fmap f4 . nonAdditiveCharacterBins)
      <*> (fmap f5 . additiveCharacterBins   )
      <*> (fmap f1 . metricCharacterBins     )
      <*> (fmap f2 . nonMetricCharacterBins  )
      <*> (fmap f6 . dynamicCharacters       )


hexsequence :: Traversable t => t (CharacterBlock m i c f a d) -> CharacterBlock (t m) (t i) (t c) (t f) (t a) (t d)
hexsequence = 
    CharacterBlock
      <$> (transposition continuousCharacterBins )
      <*> (transposition nonAdditiveCharacterBins)
      <*> (transposition additiveCharacterBins   )
      <*> (transposition metricCharacterBins     )
      <*> (transposition nonMetricCharacterBins  )
      <*> (transposition dynamicCharacters       )
  where
    transposition f xs = V.generate len g
      where
        g i = (V.! i) <$> listOfVectors
        len = length listOfVectors
        listOfVectors = fmap f xs


hexliftA2 :: (m1 -> m2 -> m3)
          -> (i1 -> i2 -> i3) 
          -> (c1 -> c2 -> c3)
          -> (f1 -> f2 -> f3)
          -> (a1 -> a2 -> a3)
          -> (d1 -> d2 -> d3)
          -> CharacterBlock m1 i1 c1 f1 a1 d1
          -> CharacterBlock m2 i2 c2 f2 a2 d2
          -> CharacterBlock m3 i3 c3 f3 a3 d3
hexliftA2 f1 f2 f3 f4 f5 f6 lhs rhs =
    CharacterBlock
        { continuousCharacterBins  = liftA2 f3 (continuousCharacterBins  lhs) (continuousCharacterBins  rhs)
        , nonAdditiveCharacterBins = liftA2 f4 (nonAdditiveCharacterBins lhs) (nonAdditiveCharacterBins rhs)
        , additiveCharacterBins    = liftA2 f5 (additiveCharacterBins    lhs) (additiveCharacterBins    rhs)
        , metricCharacterBins      = liftA2 f1 (metricCharacterBins      lhs) (metricCharacterBins      rhs)
        , nonMetricCharacterBins   = liftA2 f2 (nonMetricCharacterBins   lhs) (nonMetricCharacterBins   rhs)
        , dynamicCharacters        = liftA2 f6 (dynamicCharacters        lhs) (dynamicCharacters        rhs)
        }


instance Semigroup (CharacterBlock m i c f a d) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins  = continuousCharacterBins  lhs `mappend` continuousCharacterBins  rhs
          , nonAdditiveCharacterBins = nonAdditiveCharacterBins lhs `mappend` nonAdditiveCharacterBins rhs
          , additiveCharacterBins    = additiveCharacterBins    lhs `mappend` additiveCharacterBins    rhs
          , metricCharacterBins      = metricCharacterBins      lhs `mappend` metricCharacterBins      rhs
          , nonMetricCharacterBins   = nonMetricCharacterBins   lhs `mappend` nonMetricCharacterBins   rhs
          , dynamicCharacters        = dynamicCharacters        lhs `mappend` dynamicCharacters        rhs
          }
          

instance ( Show m
         , Show i
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (CharacterBlock m i c f a d) where

    show block = unlines
       [ "Continuous Characters: "
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "Fitch Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ nonAdditiveCharacterBins block
       , "Additive Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ additiveCharacterBins block
       , "Metric Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ metricCharacterBins block
       , "NonMetric Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ nonMetricCharacterBins block
       , "Dynamic Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ dynamicCharacters block
       ]


-- |
-- Convert all characters contained in the block to thier missing value.
toMissingCharacters :: ( PossiblyMissingCharacter m
                       , PossiblyMissingCharacter i
                       , PossiblyMissingCharacter c
                       , PossiblyMissingCharacter f
                       , PossiblyMissingCharacter a
                       , PossiblyMissingCharacter d
                       )
                    => CharacterBlock m i c f a d
                    -> CharacterBlock m i c f a d
toMissingCharacters cb =
    CharacterBlock
    { continuousCharacterBins  = toMissing <$> continuousCharacterBins  cb
    , nonAdditiveCharacterBins = toMissing <$> nonAdditiveCharacterBins cb
    , additiveCharacterBins    = toMissing <$> additiveCharacterBins    cb
    , metricCharacterBins      = toMissing <$> metricCharacterBins      cb
    , nonMetricCharacterBins   = toMissing <$> nonMetricCharacterBins   cb
    , dynamicCharacters        = toMissing <$> dynamicCharacters        cb
    }

-- TODO get rid of ContinuousDecorationInitial in signiture

-- |
-- Construct a singleton block containing a /continuous/ character.
continuousSingleton :: CharacterName -> (a -> c) -> a -> CharacterBlock m i (ContinuousDecorationInitial c) f a d
continuousSingleton nameValue transformation continuousValue =
    CharacterBlock (pure bin)  mempty  mempty  mempty mempty mempty
  where
    bin = continuousDecorationInitial nameValue transformation continuousValue


-- |
-- Construct a singleton block containing a /discrete/ character.
discreteSingleton :: TCM -> s -> CharacterBlock s s c s s d
discreteSingleton tcmValues dec =
    case tcmStructure diagnosis of
      NonSymmetric -> CharacterBlock mempty mempty mempty mempty bin    mempty
      Symmetric    -> CharacterBlock mempty mempty mempty mempty bin    mempty
      Metric       -> CharacterBlock mempty mempty mempty bin    mempty mempty
      UltraMetric  -> CharacterBlock mempty mempty mempty bin    mempty mempty
      Additive     -> CharacterBlock mempty mempty bin    mempty mempty mempty
      NonAdditive  -> CharacterBlock mempty bin    mempty mempty mempty mempty
  where
    diagnosis   = diagnoseTcm tcmValues
    bin         = pure dec


-- |
-- Construct a singleton block containing a /dynamic/ character.
dynamicSingleton :: d -> CharacterBlock m i c f a d
dynamicSingleton = CharacterBlock mempty mempty mempty mempty mempty . pure
