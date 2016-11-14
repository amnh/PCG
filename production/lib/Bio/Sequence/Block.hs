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

{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

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
import           Bio.Character.Internal
import           Bio.Metadata.CharacterName
import           Bio.Sequence.Bin.Additive
import           Bio.Sequence.Bin.Continuous
import qualified Bio.Sequence.Bin.Continuous as Continuous
import           Bio.Sequence.Bin.Metric
import           Bio.Sequence.Bin.NonAdditive
import           Bio.Sequence.Bin.NonMetric
import           Bio.Sequence.SharedContinugousMetatdata
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
data CharacterBlock s d
   = CharacterBlock
   { continuousCharacterBins   :: Maybe ContinuousBin
   , nonAdditiveCharacterBins  :: Vector (NonAdditiveBin s)
   , additiveCharacterBins     :: Vector (   AdditiveBin s)
   , metricCharacterBins       :: Vector (     MetricBin s)
   , nonNonMetricCharacterBins :: Vector (  NonMetricBin s)
   , dynamicCharacters         :: Vector (DynamicCharacterConstruct d)
   } deriving (Eq, Show)


newtype DynamicCharacterConstruct d = DCC (DiscreteCharacterMetadata, TCM, Maybe d)
  deriving (Eq, Show)


instance (EncodedAmbiguityGroupContainer s, Semigroup s) => Semigroup (CharacterBlock s d) where
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

    
toMissingCharacters :: EncodableStaticCharacterStream s => CharacterBlock s d  -> CharacterBlock s d
toMissingCharacters cb =
    CharacterBlock
    { continuousCharacterBins   =            missingContinuous <$> continuousCharacterBins   cb
    , nonAdditiveCharacterBins  = fmap (omap getMissingStatic) <$> nonAdditiveCharacterBins  cb
    , additiveCharacterBins     = fmap (omap getMissingStatic) <$> additiveCharacterBins     cb
    , metricCharacterBins       = fmap (omap getMissingStatic) <$> metricCharacterBins       cb
    , nonNonMetricCharacterBins = fmap (omap getMissingStatic) <$> nonNonMetricCharacterBins cb
    , dynamicCharacters         =               missingDynamic <$> dynamicCharacters         cb
    }
  where
    missingContinuous x =
        ContinuousBin
        { Continuous.characterStream = fmap (const Nothing) . Continuous.characterStream $ x
        , Continuous.metatdataBounds =                        Continuous.metatdataBounds $ x
        }
    missingDynamic (DCC (gcm, tcm, _)) = DCC (gcm, tcm, Nothing)


continuousSingleton :: CharacterName -> (Maybe Double) -> CharacterBlock s d
continuousSingleton nameValue continuousValue =
    CharacterBlock (Just bin)  mempty  mempty  mempty mempty mempty
  where
    bin      = continuousBin (continuousValue :| []) metadata
    metadata = continuousMetadata nameValue 1


discreteSingleton :: Alphabet String -> CharacterName -> TCM -> (a -> s) -> a -> CharacterBlock s d
discreteSingleton alphabetValues nameValue tcmValues transformation input =
  case tcmStructure diagnosis of
    NonSymetric -> (\x -> CharacterBlock Nothing  mempty  mempty  mempty (pure x) mempty) $   NonMetricBin character metadata $ factoredTcm diagnosis
    Symetric    -> (\x -> CharacterBlock Nothing  mempty  mempty  mempty (pure x) mempty) $   NonMetricBin character metadata $ factoredTcm diagnosis
    Metric      -> (\x -> CharacterBlock Nothing  mempty  mempty (pure x) mempty  mempty) $      MetricBin character metadata $ factoredTcm diagnosis
    UltraMetric -> (\x -> CharacterBlock Nothing  mempty  mempty (pure x) mempty  mempty) $      MetricBin character metadata $ factoredTcm diagnosis
    Additive    -> (\x -> CharacterBlock Nothing  mempty (pure x) mempty  mempty  mempty) $    AdditiveBin character metadata --- $ symbolCount character
    NonAdditive -> (\x -> CharacterBlock Nothing (pure x) mempty  mempty  mempty  mempty) $ NonAdditiveBin character metadata --- $ symbolCount character
  where
    character = transformation input
    diagnosis = diagnoseTcm tcmValues
    metadata  = singleton 1 . discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis

-- DCC (DiscreteCharacterMetadata, TCM, Maybe d)

dynamicSingleton :: Alphabet String -> CharacterName -> TCM -> (a -> d) -> Maybe a -> CharacterBlock s d
dynamicSingleton alphabetValues nameValue tcmValues transformation input =
    CharacterBlock Nothing mempty mempty mempty mempty . pure $ DCC (metadata, tcmValues, character)
  where
    character = transformation <$> input
    diagnosis = diagnoseTcm tcmValues
    metadata  = discreteMetadata alphabetValues nameValue . fromIntegral $ factoredWeight diagnosis

    
