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


import Bio.Character
import Bio.Character.Encodable
import Bio.Character.Decoration.Continuous
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Dynamic
import Bio.Metadata.CharacterName
import Data.Alphabet
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
   } deriving (Eq, Show)


instance ( EncodedAmbiguityGroupContainer m, Semigroup m
         , EncodedAmbiguityGroupContainer i, Semigroup i
         , Semigroup c
         , EncodedAmbiguityGroupContainer f, Semigroup f
         , EncodedAmbiguityGroupContainer a, Semigroup a
         ) => Semigroup (CharacterBlock m i c f a d) where
    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins   = continuousCharacterBins   lhs `mappend` continuousCharacterBins   rhs
          , nonAdditiveCharacterBins  = nonAdditiveCharacterBins  lhs `mappend` nonAdditiveCharacterBins  rhs
          , additiveCharacterBins     = additiveCharacterBins     lhs `mappend` additiveCharacterBins     rhs
          , metricCharacterBins       = metricCharacterBins       lhs `mappend` metricCharacterBins       rhs
          , nonNonMetricCharacterBins = nonNonMetricCharacterBins lhs `mappend` nonNonMetricCharacterBins rhs
          , dynamicCharacters         = dynamicCharacters         lhs `mappend` dynamicCharacters         rhs
          }

    
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


discreteSingleton :: EncodableStaticCharacter s
                  => Alphabet String -> CharacterName -> TCM -> (a -> s) -> a
                  -> CharacterBlock (DiscreteDecoration s) (DiscreteDecoration s) c (DiscreteDecoration s) (DiscreteDecoration s) d
discreteSingleton alphabetValues nameValue tcmValues transformation input =
    case tcmStructure diagnosis of
      NonSymmetric -> CharacterBlock mempty mempty mempty mempty bin    mempty
      Symmetric    -> CharacterBlock mempty mempty mempty mempty bin    mempty
      Metric       -> CharacterBlock mempty mempty mempty bin    mempty mempty
      UltraMetric  -> CharacterBlock mempty mempty mempty bin    mempty mempty
      Additive     -> CharacterBlock mempty mempty bin    mempty mempty mempty
      NonAdditive  -> CharacterBlock mempty bin    mempty mempty mempty mempty
  where
    diagnosis   = diagnoseTcm tcmValues
    weightValue = fromIntegral $ factoredWeight diagnosis
    bin         = pure $ toDiscreteCharacterDecoration nameValue weightValue alphabetValues (factoredTcm diagnosis) transformation input


dynamicSingleton :: EncodableDynamicCharacter d
                 => Alphabet String -> CharacterName -> TCM -> (x -> d) -> x -> CharacterBlock m i c f a (DynamicDecorationInitial d)
dynamicSingleton alphabetValues nameValue tcmValues transformation input =
    CharacterBlock mempty mempty mempty mempty mempty bin
  where
    diagnosis   = diagnoseTcm tcmValues
    weightValue = fromIntegral $ factoredWeight diagnosis
    bin         = pure $ toDynamicCharacterDecoration nameValue weightValue alphabetValues (factoredTcm diagnosis) transformation input

    
