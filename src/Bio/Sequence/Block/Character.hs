------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Block.Character
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Bio.Sequence.Block.Character
  ( CharacterBlock(..)
  -- * Block construction
  , finalizeCharacterBlock
  , continuousSingleton
  , discreteSingleton
  , dynamicSingleton
  -- * Block extraction
  , continuousCharacterBins
  , nonAdditiveCharacterBins
  , additiveCharacterBins
  , metricCharacterBins
  , nonMetricCharacterBins
  , dynamicCharacters
  , setDynamicCharacters
  -- * Transformations
  , hexmap
  , hexTranspose
  , hexZipWith
  , hexZipWithMeta
  , toMissingCharacters
  ) where


import           Bio.Character.Encodable
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence.Block.Builder
import           Bio.Sequence.Block.Internal
import           Bio.Sequence.Block.Metadata         (MetadataBlock(..))
--import qualified Bio.Sequence.Block.Metadata as Meta
import           Control.DeepSeq
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.Vector                         (Vector, fromList)
import qualified Data.Vector                 as V
import           Data.Vector.Instances               ()
import           Data.Void
import           GHC.Generics
import           Text.XML
-- import Text.XML.Light.Types


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype CharacterBlock u v w x y z = CB (Block Void u v w x y z)
    deriving (Bifunctor, Eq, Functor, NFData, Generic, Semigroup)


-- | (✔)
instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (CharacterBlock u v w x y z) where

    show (CB block) = unlines
        [ "Non-additive s:"
        , niceRendering $ nonAdditiveBins block
        , "Additive s:"
        , niceRendering $ additiveBins block
        , "NonMetric s:"
        , niceRendering $ nonMetricBins block
        , "Continuous s: "
        , niceRendering $ continuousBins block
        , "Metric s:"
        , niceRendering $ metricBins block
        , "Dynamic s:"
        , niceRendering $ dynamicBins block
        ]
      where
        niceRendering :: (Foldable t, Show a) => t a -> String
        niceRendering = unlines . fmap (unlines . fmap ("  " <>) . lines . show) . toList


-- | (✔)
instance ( ToXML u -- This is NOT a redundant constraint.
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterBlock u v w x y z) where

    toXML (CB block) = xmlElement "_block" attributes contents
        where
            attributes = []
            contents   = [ Right . collapseElemList "Non-additive_character_block" [] $ nonAdditiveBins block
                         , Right . collapseElemList "Additive_character_block"     [] $ additiveBins    block
                         , Right . collapseElemList "NonMetric_character_block"    [] $ nonMetricBins   block
                         , Right . collapseElemList "Continuous_character_block"   [] $ continuousBins  block
                         , Right . collapseElemList "Metric_character_block"       [] $ nonMetricBins   block
                         , Right . collapseElemList "Dynamic_character_block"      [] $ dynamicBins        block
                         ]


-- |
-- Converts a 'PartialCharacterBlock' to a 'CharacterBlock', finalizing the
-- efficient construction process.
finalizeCharacterBlock :: PartialCharacterBlock u v w x y z -> CharacterBlock u v w x y z
finalizeCharacterBlock = CB . (
    Block
      <$> const undefined
      <*> fromDList . partialContinuousCharacterBins
      <*> fromDList . partialNonAdditiveCharacterBins
      <*> fromDList . partialAdditiveCharacterBins
      <*> fromDList . partialMetricCharacterBins
      <*> fromDList . partialNonMetricCharacterBins
      <*> fromDList . partialDynamicCharacters
    )
  where
    fromDList = fromList . toList


continuousCharacterBins :: CharacterBlock u v w x y z -> Vector u
continuousCharacterBins = continuousBins . unwrap


nonAdditiveCharacterBins :: CharacterBlock u v w x y z -> Vector v
nonAdditiveCharacterBins = nonAdditiveBins . unwrap


additiveCharacterBins :: CharacterBlock u v w x y z -> Vector w
additiveCharacterBins = additiveBins . unwrap


metricCharacterBins :: CharacterBlock u v w x y z -> Vector x
metricCharacterBins = metricBins . unwrap


nonMetricCharacterBins :: CharacterBlock u v w x y z -> Vector y
nonMetricCharacterBins = nonMetricBins . unwrap


dynamicCharacters :: CharacterBlock u v w x y z -> Vector z
dynamicCharacters = dynamicBins . unwrap


setDynamicCharacters :: Vector z -> CharacterBlock u v w x y a -> CharacterBlock u v w x y z
setDynamicCharacters v = CB . (
    Block
      <$> const undefined
      <*> continuousCharacterBins
      <*> nonAdditiveCharacterBins
      <*> additiveCharacterBins
      <*> metricCharacterBins
      <*> nonMetricCharacterBins
      <*> const v
    )


-- |
-- Perform a six way map over the polymorphic types.
hexmap
 :: (u -> u')
 -> (v -> v')
 -> (w -> w')
 -> (x -> x')
 -> (y -> y')
 -> (z -> z')
 -> CharacterBlock u  v  w  x  y  z
 -> CharacterBlock u' v' w' x' y' z'
hexmap f1 f2 f3 f4 f5 f6 = CB . (
    Block
      <$> const undefined
      <*> (parmap rpar f1 . continuousCharacterBins )
      <*> (parmap rpar f2 . nonAdditiveCharacterBins)
      <*> (parmap rpar f3 . additiveCharacterBins   )
      <*> (parmap rpar f4 . metricCharacterBins     )
      <*> (parmap rpar f5 . nonMetricCharacterBins  )
      <*> (parmap rpar f6 . dynamicCharacters       )
    )

-- |
-- Performs a 2D transform on the 'Traversable' structure of 'CharacterBlock'
-- values.
-- 
-- Assumes that the 'CharacterBlock' values in the 'Traversable' structure are of
-- equal length. If this assumtion is violated, the result will be truncated.
hexTranspose 
  :: Traversable t 
  => t (CharacterBlock u v w x y z) 
  -> CharacterBlock (t u) (t v) (t w) (t x) (t y) (t z)
hexTranspose = CB . (
    Block
      <$> const undefined
      <*> transposition continuousCharacterBins
      <*> transposition nonAdditiveCharacterBins
      <*> transposition additiveCharacterBins
      <*> transposition metricCharacterBins
      <*> transposition nonMetricCharacterBins
      <*> transposition dynamicCharacters
    )
  where
    transposition f xs =
        case toList listOfVectors of
          [] -> mempty
          ys -> V.generate (length ys) g
      where
        g i = (V.! i) <$> listOfVectors
        listOfVectors = fmap f xs


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
-- 
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipWith 
  :: (u -> u' -> u'')
  -> (v -> v' -> v'') 
  -> (w -> w' -> w'')
  -> (x -> x' -> x'')
  -> (y -> y' -> y'')
  -> (z -> z' -> z'')
  -> CharacterBlock u   v   w   x   y   z
  -> CharacterBlock u'  v'  w'  x'  y'  z'
  -> CharacterBlock u'' v'' w'' x'' y'' z''
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs = CB $
    Block
      { blockMetadata   = undefined
      , continuousBins  = parZipWith rpar f1 (continuousCharacterBins  lhs) (continuousCharacterBins  rhs)
      , nonAdditiveBins = parZipWith rpar f2 (nonAdditiveCharacterBins lhs) (nonAdditiveCharacterBins rhs)
      , additiveBins    = parZipWith rpar f3 (additiveCharacterBins    lhs) (additiveCharacterBins    rhs)
      , metricBins      = parZipWith rpar f4 (metricCharacterBins      lhs) (metricCharacterBins      rhs)
      , nonMetricBins   = parZipWith rpar f5 (nonMetricCharacterBins   lhs) (nonMetricCharacterBins   rhs)
      , dynamicBins     = parZipWith rpar f6 (dynamicCharacters        lhs) (dynamicCharacters        rhs)
      }


-- |
-- Performs a zip over the two character blocks. Uses the input functions to zip
-- the different character types in the character block.
-- 
-- Assumes that the 'CharacterBlock' values have the same number of each character
-- type. If this assumtion is violated, the result will be truncated.
hexZipWithMeta
  :: (ContinuousCharacterMetadataDec        -> u -> u' -> u'')
  -> (DiscreteCharacterMetadataDec          -> v -> v' -> v'') 
  -> (DiscreteCharacterMetadataDec          -> w -> w' -> w'')
  -> (DiscreteWithTCMCharacterMetadataDec e -> x -> x' -> x'')
  -> (DiscreteWithTCMCharacterMetadataDec e -> y -> y' -> y'')
  -> (DynamicCharacterMetadataDec d         -> z -> z' -> z'')
  -> MetadataBlock  e d m
  -> CharacterBlock u   v   w   x   y   z
  -> CharacterBlock u'  v'  w'  x'  y'  z'
  -> CharacterBlock u'' v'' w'' x'' y'' z''
hexZipWithMeta f1 f2 f3 f4 f5 f6 (MB meta) (CB lhs) (CB rhs) = CB $
    Block
      { blockMetadata   = undefined
      , continuousBins  = parZipWith3 rpar f1 (continuousBins  meta) (continuousBins  lhs) (continuousBins  rhs)
      , nonAdditiveBins = parZipWith3 rpar f2 (nonAdditiveBins meta) (nonAdditiveBins lhs) (nonAdditiveBins rhs)
      , additiveBins    = parZipWith3 rpar f3 (additiveBins    meta) (additiveBins    lhs) (additiveBins    rhs)
      , metricBins      = parZipWith3 rpar f4 (metricBins      meta) (metricBins      lhs) (metricBins      rhs)
      , nonMetricBins   = parZipWith3 rpar f5 (nonMetricBins   meta) (nonMetricBins   lhs) (nonMetricBins   rhs)
      , dynamicBins     = parZipWith3 rpar f6 (dynamicBins     meta) (dynamicBins     lhs) (dynamicBins     rhs)
      }


-- |
-- Convert all characters contained in the block to thier missing value.
toMissingCharacters 
  :: ( PossiblyMissingCharacter u
     , PossiblyMissingCharacter v
     , PossiblyMissingCharacter w
     , PossiblyMissingCharacter x
     , PossiblyMissingCharacter y 
     , PossiblyMissingCharacter z
     )
  => CharacterBlock u v w x y z
  -> CharacterBlock u v w x y z
toMissingCharacters = CB . (
    Block
      <$> const undefined
      <*> (fmap toMissing . continuousCharacterBins)
      <*> (fmap toMissing . nonAdditiveCharacterBins)
      <*> (fmap toMissing . additiveCharacterBins)
      <*> (fmap toMissing . metricCharacterBins)
      <*> (fmap toMissing . nonMetricCharacterBins)
      <*> (fmap toMissing . dynamicCharacters)
    )


{-# INLINE unwrap #-}
unwrap :: CharacterBlock u v w x y z -> Block Void u v w x y z
unwrap (CB x) = x
