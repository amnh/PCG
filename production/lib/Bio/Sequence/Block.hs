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

{-# LANGUAGE FlexibleContexts #-}

module Bio.Sequence.Block
  ( CharacterBlock(..)
  , toMissingCharacters
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
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
   { continuousCharacterBins   :: Vector c
   , nonAdditiveCharacterBins  :: Vector f
   , additiveCharacterBins     :: Vector a
   , metricCharacterBins       :: Vector m
   , nonNonMetricCharacterBins :: Vector i
   , dynamicCharacters         :: Vector d
   } deriving (Eq)


instance Semigroup (CharacterBlock m i c f a d) where

    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins   = continuousCharacterBins   lhs `mappend` continuousCharacterBins   rhs
          , nonAdditiveCharacterBins  = nonAdditiveCharacterBins  lhs `mappend` nonAdditiveCharacterBins  rhs
          , additiveCharacterBins     = additiveCharacterBins     lhs `mappend` additiveCharacterBins     rhs
          , metricCharacterBins       = metricCharacterBins       lhs `mappend` metricCharacterBins       rhs
          , nonNonMetricCharacterBins = nonNonMetricCharacterBins lhs `mappend` nonNonMetricCharacterBins rhs
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
       [ "Continuous Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "Fitch Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "Additive Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "Metric Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "NonMetric Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       , "Dynamic Characters:"
       , unlines . fmap (("  " <>) . show) . toList $ continuousCharacterBins block
       ]


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
    { continuousCharacterBins   = toMissing <$> continuousCharacterBins   cb
    , nonAdditiveCharacterBins  = toMissing <$> nonAdditiveCharacterBins  cb
    , additiveCharacterBins     = toMissing <$> additiveCharacterBins     cb
    , metricCharacterBins       = toMissing <$> metricCharacterBins       cb
    , nonNonMetricCharacterBins = toMissing <$> nonNonMetricCharacterBins cb
    , dynamicCharacters         = toMissing <$> dynamicCharacters         cb
    }


continuousSingleton :: CharacterName -> (a -> c) -> a -> CharacterBlock m i (ContinuousDecorationInitial c) f a d
continuousSingleton nameValue transformation continuousValue =
    CharacterBlock (pure bin)  mempty  mempty  mempty mempty mempty
  where
    bin = continuousDecorationInitial nameValue transformation continuousValue


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


dynamicSingleton :: d -> CharacterBlock m i c f a d
dynamicSingleton = CharacterBlock mempty mempty mempty mempty mempty . pure
