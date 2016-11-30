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
  , AdditiveBin()
  , ContinuousBin()
  , MetricBin()
  , NonAdditiveBin()
  , NonMetricBin()
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


newtype DynamicCharacterConstruct d = DCC (DiscreteCharacterMetadataDec d, TCM, Maybe d)
  deriving (Eq, Show)

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
                       ) 
                    => CharacterBlock m i c f a d
                    -> CharacterBlock m i c f a d
toMissingCharacters cb =
    CharacterBlock
    { continuousCharacterBins   =        toMissing <$> continuousCharacterBins   cb
    , nonAdditiveCharacterBins  =        toMissing <$> nonAdditiveCharacterBins  cb
    , additiveCharacterBins     =        toMissing <$> additiveCharacterBins     cb
    , metricCharacterBins       =        toMissing <$> metricCharacterBins       cb
    , nonNonMetricCharacterBins =        toMissing <$> nonNonMetricCharacterBins cb
    , dynamicCharacters         =   missingDynamic <$> dynamicCharacters         cb
    }
  where
--    encodableStreamToMissing = fmap (omap getMissingStatic)
    missingContinuous x = (Nothing <$)
    missingDynamic (DCC (gcm, tcm, _)) = DCC (gcm, tcm, Nothing)


continuousSingleton :: Real r => CharacterName -> Maybe r -> (Maybe r -> c) -> CharacterBlock m i (ContinuousDecorationInitial c) f a d
continuousSingleton nameValue continuousValue f =
    CharacterBlock (pure bin)  mempty  mempty  mempty mempty mempty
  where
    bin      = continuousDecorationInitial nameValue continuousValue f


discreteSingleton :: Alphabet String -> CharacterName -> TCM -> (a -> s) -> a -> CharacterBlock s s c s s d
discreteSingleton alphabetValues nameValue tcmValues transformation input =
  case tcmStructure diagnosis of
    NonSymmetric -> (\x -> CharacterBlock Nothing  mempty  mempty  mempty (pure x) mempty) .   NonMetricBin character metadata $ factoredTcm diagnosis
    Symmetric    -> (\x -> CharacterBlock Nothing  mempty  mempty  mempty (pure x) mempty) .   NonMetricBin character metadata $ factoredTcm diagnosis
    Metric       -> (\x -> CharacterBlock Nothing  mempty  mempty (pure x) mempty  mempty) .      MetricBin character metadata $ factoredTcm diagnosis
    UltraMetric  -> (\x -> CharacterBlock Nothing  mempty  mempty (pure x) mempty  mempty) .      MetricBin character metadata $ factoredTcm diagnosisschachter
    Additive     -> (\x -> CharacterBlock Nothing  mempty (pure x) mempty  mempty  mempty) $    AdditiveBin character metadata --- $ symbolCount character
    NonAdditive  -> (\x -> CharacterBlock Nothing (pure x) mempty  mempty  mempty  mempty) $ NonAdditiveBin character metadata --- $ symbolCount character
  where
    character = transformation input
    diagnosis = diagnoseTcm tcmValues
    metadata  = singleton 1 . discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis

-- DCC (DiscreteCharacterMetadata, TCM, Maybe d)

dynamicSingleton :: Alphabet String -> CharacterName -> TCM -> (x -> d) -> Maybe x -> CharacterBlock m i c f a d
dynamicSingleton alphabetValues nameValue tcmValues transformation input =
    CharacterBlock Nothing mempty mempty mempty mempty . pure $ DCC (metadata, tcmValues, character)
  where
    character = transformation <$> input
    diagnosis = diagnoseTcm tcmValues
    metadata  = discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis

    
