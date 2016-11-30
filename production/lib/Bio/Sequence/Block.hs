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
--  , AdditiveBin()
--  , ContinuousBin()
--  , MetricBin()
--  , NonAdditiveBin()
--  , NonMetricBin()
  , toMissingCharacters
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  ) where


import           Bio.Character
import           Bio.Character.Encodable
import           Bio.Metadata.CharacterName
import           Bio.Metadata.Discrete
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import qualified Bio.Character.Decoration.Continuous as Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Metric
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.NonMetric
import           Data.Alphabet
import           Data.Foldable
import           Data.List.NonEmpty                 (NonEmpty( (:|) ))
import           Data.List.Zipper            hiding (toList)
import qualified Data.List.Zipper            as Zip
import           Data.Monoid                        (mappend)
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.TCM
import           Data.Vector                        (Vector)
import qualified Data.Vector                 as V


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


{-
instance ( EncodedAmbiguityGroupContainer m, Semigroup m
         , EncodedAmbiguityGroupContainer i, Semigroup i
         , Semigroup c
         , EncodedAmbiguityGroupContainer f, Semigroup f
         , EncodedAmbiguityGroupContainer a, Semigroup a
         ) => Semigroup (CharacterBlock m i c f a d) where
    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins   = continuousCharacterBins lhs `maybeMerge` continuousCharacterBins rhs
          , nonAdditiveCharacterBins  = mergeByComparing symbolCount ( nonAdditiveCharacterBins lhs) ( nonAdditiveCharacterBins rhs)
          , additiveCharacterBins     = mergeByComparing symbolCount (    additiveCharacterBins lhs) (    additiveCharacterBins rhs)
          , metricCharacterBins       = mergeByComparing symbolCount (      metricCharacterBins lhs) (      metricCharacterBins rhs)
          , nonNonMetricCharacterBins = mergeByComparing symbolCount (nonNonMetricCharacterBins lhs) (nonNonMetricCharacterBins rhs)
          , dynamicCharacters         = dynamicCharacters lhs `mappend` dynamicCharacters rhs
          }
      where
        maybeMerge x y =
          case x of
            Nothing -> y
            Just v  ->
              case y of
                Nothing -> x
                Just w  -> Just $ v <> w
-}

mergeByComparing :: (Eq a, Semigroup s) => (s -> a) -> Vector s -> Vector s -> Vector s
mergeByComparing comparator lhs rhs
    | null lhs  = rhs
    | null rhs  = lhs
    | otherwise = lhs' `mappend` (V.fromList . Zip.toList) rhs'
    where
      (rhs', lhs') = foldl' f initialAccumulator lhs
      initialAccumulator = (Zip.fromList $ toList rhs, mempty)
      f (x,y) e = g x
        where
          g z =
            case safeCursor z of
              Nothing -> (start z, y `mappend` pure e)
              Just a  ->
                if comparator a == comparator e
                then (start $ delete z, y `mappend` pure (e <> a))
                else g (right z)

    
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
--  where
--    encodableStreamToMissing = fmap (omap getMissingStatic)
--    missingContinuous x = (Nothing <$)
--    missingDynamic (DCC (gcm, tcm, _)) = DCC (gcm, tcm, Nothing)


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
    character   = transformation input
    diagnosis   = diagnoseTcm tcmValues
    weightValue = fromIntegral $ factoredWeight diagnosis
--    metadata    = singleton 1 . discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis
    bin         = pure $ toDiscreteCharacterDecoration nameValue weightValue alphabetValues (factoredTcm diagnosis) transformation input

-- DCC (DiscreteCharacterMetadata, TCM, Maybe d)

dynamicSingleton :: EncodableDynamicCharacter d
                 => Alphabet String -> CharacterName -> TCM -> (x -> d) -> x -> CharacterBlock m i c f a (DynamicDecorationInitial d)
dynamicSingleton alphabetValues nameValue tcmValues transformation input =
    CharacterBlock mempty mempty mempty mempty mempty bin -- . pure $ DCC (metadata, tcmValues, character)
  where
    character   = transformation input
    diagnosis   = diagnoseTcm tcmValues
    weightValue = fromIntegral $ factoredWeight diagnosis
--    metadata  = discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis
    bin         = pure $ toDynamicCharacterDecoration nameValue weightValue alphabetValues (factoredTcm diagnosis) transformation input

    
