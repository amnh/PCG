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
  , hexmap
  ) where


import Bio.Character.Encodable
import Bio.Character.Decoration.Continuous
import Bio.Metadata.CharacterName
import Data.Foldable
import Data.Monoid                         (mappend)
import Data.Semigroup
import Data.TCM
import Data.Vector                         (Vector)


-- |
-- Represents a block of charcters which are optimized atomically together across
-- networks. The 'CharcterBlock' is polymorphic over static and dynamic charcter
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


instance Semigroup (CharacterBlock m i c f a d) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins   = continuousCharacterBins   lhs `mappend` continuousCharacterBins   rhs
          , nonAdditiveCharacterBins  = nonAdditiveCharacterBins  lhs `mappend` nonAdditiveCharacterBins  rhs
          , additiveCharacterBins     = additiveCharacterBins     lhs `mappend` additiveCharacterBins     rhs
          , metricCharacterBins       = metricCharacterBins       lhs `mappend` metricCharacterBins       rhs
          , nonMetricCharacterBins = nonMetricCharacterBins lhs `mappend` nonMetricCharacterBins rhs
          , dynamicCharacters         = dynamicCharacters         lhs `mappend` dynamicCharacters         rhs
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
