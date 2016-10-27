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
  ) where


import           Bio.Character
import           Bio.Character.Internal
import           Bio.Sequence.Bin.Additive
import           Bio.Sequence.Bin.Continuous
import qualified Bio.Sequence.Bin.Continuous as Continuous
import           Bio.Sequence.Bin.Metric
import           Bio.Sequence.Bin.NonAdditive
import           Bio.Sequence.Bin.NonMetric
import           Bio.Sequence.SharedContinugousMetatdata
import           Data.Foldable
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
   { continuousCharacterBins   :: ContinuousBin
   , nonAdditiveCharacterBins  :: Vector (NonAdditiveBin s)
   , additiveCharacterBins     :: Vector (   AdditiveBin s)
   , metricCharacterBins       :: Vector (     MetricBin s)
   , nonNonMetricCharacterBins :: Vector (  NonMetricBin s)
   , dynamicCharacters         :: Vector (DynamicCharacterConstruct d)
   } deriving (Eq, Show)


newtype DynamicCharacterConstruct d = DCC (GeneralCharacterMetadata, TCM, Maybe d)
  deriving (Eq, Show)


instance Semigroup s => Semigroup (CharacterBlock s d) where
    lhs <> rhs =
        CharacterBlock
          { continuousCharacterBins   = continuousCharacterBins lhs <> continuousCharacterBins rhs
          , nonAdditiveCharacterBins  = mergeByComparing symbolCount ( nonAdditiveCharacterBins lhs) ( nonAdditiveCharacterBins rhs)
          , additiveCharacterBins     = mergeByComparing symbolCount (    additiveCharacterBins lhs) (    additiveCharacterBins rhs)
          , metricCharacterBins       = mergeByComparing symbolCount (      metricCharacterBins lhs) (      metricCharacterBins rhs)
          , nonNonMetricCharacterBins = mergeByComparing symbolCount (nonNonMetricCharacterBins lhs) (nonNonMetricCharacterBins rhs)
          , dynamicCharacters         = dynamicCharacters lhs `mappend` dynamicCharacters rhs
          }


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
    { continuousCharacterBins   =           missingContinuous   $  continuousCharacterBins   cb
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
