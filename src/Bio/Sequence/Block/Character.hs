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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bio.Sequence.Block.Character
  ( CharacterBlock(..)
  -- * Lenses
  , HasBlockMetadata(..)
  , HasContinuousBin(..)
  , HasNonAdditiveBin(..)
  , HasAdditiveBin(..)
  , HasMetricBin(..)
  , HasNonMetricBin(..)
  , HasDynamicBin(..)
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
--  , setDynamicCharacters
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
import           Bio.Sequence.Block.Metadata  (MetadataBlock (..))
import           Control.DeepSeq
import           Control.Lens
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.MonoTraversable         (Element)
import           Data.Vector                  (Vector, fromList)
import qualified Data.Vector                  as V
import           Data.Vector.Instances        ()
import           GHC.Generics
import           Text.XML


-- |
-- Represents a block of data which are optimized atomically together across
-- networks.
--
-- Use '(<>)' to construct larger blocks.
newtype CharacterBlock u v w x y z = CB { unwrap :: (Block u v w x y z) }
    deriving (Bifunctor, Eq, Functor, Generic, NFData, Semigroup)


instance HasContinuousBin (CharacterBlock u v w x y z) (Vector u) where

    {-# INLINE continuousBin #-}
    continuousBin = lens (_continuousBin . unwrap)
                  $ \(CB b) x -> CB (b { _continuousBin = x })


instance HasNonAdditiveBin (CharacterBlock u v w x y z) (Vector v) where

    {-# INLINE nonAdditiveBin #-}
    nonAdditiveBin = lens (_nonAdditiveBin . unwrap)
                   $ \(CB b) x -> CB (b { _nonAdditiveBin = x })


instance HasAdditiveBin (CharacterBlock u v w x y z) (Vector w) where

    {-# INLINE additiveBin #-}
    additiveBin = lens (_additiveBin . unwrap)
                $ \(CB b) x -> CB (b { _additiveBin = x })


instance HasMetricBin (CharacterBlock u v w x y z) (Vector x) where

    {-# INLINE metricBin #-}
    metricBin = lens (_metricBin . unwrap)
              $ \(CB b) x -> CB (b { _metricBin = x })


instance HasNonMetricBin (CharacterBlock u v w x y z) (Vector y) where

    {-# INLINE nonMetricBin #-}
    nonMetricBin = lens (_nonMetricBin . unwrap)
                 $ \(CB b) x -> CB (b { _nonMetricBin = x })


instance HasDynamicBin (CharacterBlock u v w x y z) (CharacterBlock u v w x y z') (Vector z) (Vector z') where

    {-# INLINE  dynamicBin #-}
    dynamicBin = lens (_dynamicBin . unwrap)
               $ \(CB b) x -> CB (b { _dynamicBin = x })


-- | (✔)
instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         ) => Show (CharacterBlock u v w x y z) where

    show (CB block) = unlines
        [ "Continuous s [" <> show (length (_continuousBin block)) <> "]:"
        , niceRendering $ _continuousBin block
        , "Non-additive s [" <> show (length (_nonAdditiveBin block)) <> "]:"
        , niceRendering $ _nonAdditiveBin block
        , "Additive s [" <> show (length (_additiveBin block)) <> "]:"
        , niceRendering $ _additiveBin block
        , "Metric s [" <> show (length (_metricBin block)) <> "]:"
        , niceRendering $ _metricBin block
        , "NonMetric s [" <> show (length (_nonMetricBin block)) <> "]:"
        , niceRendering $ _nonMetricBin block
        , "Dynamic s [" <> show (length (_dynamicBin block)) <> "]:"
        , niceRendering $ _dynamicBin block
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
            contents   = [ Right . collapseElemList "Continuous_character_block"   [] . (^.  continuousBin)
                         , Right . collapseElemList "Non-additive_character_block" [] . (^. nonAdditiveBin)
                         , Right . collapseElemList "Additive_character_block"     [] . (^.    additiveBin)
                         , Right . collapseElemList "NonMetric_character_block"    [] . (^.   nonMetricBin)
                         , Right . collapseElemList "Metric_character_block"       [] . (^.   nonMetricBin)
                         , Right . collapseElemList "Dynamic_character_block"      [] . (^.     dynamicBin)
                         ] <*> [block]


-- |
-- Converts a 'PartialCharacterBlock' to a 'CharacterBlock', finalizing the
-- efficient construction process.
finalizeCharacterBlock :: PartialCharacterBlock u v w x y z -> CharacterBlock u v w x y z
finalizeCharacterBlock = CB . (
    Block
      <$> fromDList . partialContinuousCharacterBins
      <*> fromDList . partialNonAdditiveCharacterBins
      <*> fromDList . partialAdditiveCharacterBins
      <*> fromDList . partialMetricCharacterBins
      <*> fromDList . partialNonMetricCharacterBins
      <*> fromDList . partialDynamicCharacters
    )
  where
    fromDList = fromList . toList


continuousCharacterBins :: CharacterBlock u v w x y z -> Vector u
continuousCharacterBins = (^. continuousBin) . unwrap


nonAdditiveCharacterBins :: CharacterBlock u v w x y z -> Vector v
nonAdditiveCharacterBins = (^. nonAdditiveBin) . unwrap


additiveCharacterBins :: CharacterBlock u v w x y z -> Vector w
additiveCharacterBins = (^. additiveBin) . unwrap


metricCharacterBins :: CharacterBlock u v w x y z -> Vector x
metricCharacterBins = (^. metricBin) . unwrap


nonMetricCharacterBins :: CharacterBlock u v w x y z -> Vector y
nonMetricCharacterBins = (^. nonMetricBin) . unwrap


dynamicCharacters :: CharacterBlock u v w x y z -> Vector z
dynamicCharacters = (^. dynamicBin) . unwrap


{-
setDynamicCharacters :: Vector z -> CharacterBlock u v w x y a -> CharacterBlock u v w x y z
setDynamicCharacters v = CB . (
    Block
      <$> continuousCharacterBins
      <*> nonAdditiveCharacterBins
      <*> additiveCharacterBins
      <*> metricCharacterBins
      <*> nonMetricCharacterBins
      <*> const v
    )
-}


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
      <$> (parmap rpar f1 . continuousCharacterBins )
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
      <$> transposition continuousCharacterBins
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
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs = CB
    Block
      { _continuousBin  = parZipWith rpar f1 (continuousCharacterBins  lhs) (continuousCharacterBins  rhs)
      , _nonAdditiveBin = parZipWith rpar f2 (nonAdditiveCharacterBins lhs) (nonAdditiveCharacterBins rhs)
      , _additiveBin    = parZipWith rpar f3 (additiveCharacterBins    lhs) (additiveCharacterBins    rhs)
      , _metricBin      = parZipWith rpar f4 (metricCharacterBins      lhs) (metricCharacterBins      rhs)
      , _nonMetricBin   = parZipWith rpar f5 (nonMetricCharacterBins   lhs) (nonMetricCharacterBins   rhs)
      , _dynamicBin     = parZipWith rpar f6 (dynamicCharacters        lhs) (dynamicCharacters        rhs)
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
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> x -> x' -> x'')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> y -> y' -> y'')
  -> (DynamicCharacterMetadataDec (Element DynamicChar) -> z -> z' -> z'')
  -> MetadataBlock m
  -> CharacterBlock u   v   w   x   y   z
  -> CharacterBlock u'  v'  w'  x'  y'  z'
  -> CharacterBlock u'' v'' w'' x'' y'' z''
{-
hexZipWithMeta _ _ _ _ _ _ (CB meta) (CB lhs) (CB rhs)
  | trace ( unlines
              [ "\n()()() INPUTs to hexZipWithMeta:"
              , "  Metadata Sequence:"
              , "    " <> (show . length . dynamicBins) meta
              , "  Left-hand  side Character Sequence:"
              , "    " <> (show . length . dynamicBins) lhs
              , "  Right-hand side Character Sequence:"
              , "    " <> (show . length . dynamicBins) rhs
              ]
          )
    False = undefined
-}
hexZipWithMeta f1 f2 f3 f4 f5 f6 (MB _ meta) (CB lhs) (CB rhs) = CB
    Block
      { _continuousBin  = parZipWith3 rpar f1 (_continuousBin  meta) (_continuousBin  lhs) (_continuousBin  rhs)
      , _nonAdditiveBin = parZipWith3 rpar f2 (_nonAdditiveBin meta) (_nonAdditiveBin lhs) (_nonAdditiveBin rhs)
      , _additiveBin    = parZipWith3 rpar f3 (_additiveBin    meta) (_additiveBin    lhs) (_additiveBin    rhs)
      , _metricBin      = parZipWith3 rpar f4 (_metricBin      meta) (_metricBin      lhs) (_metricBin      rhs)
      , _nonMetricBin   = parZipWith3 rpar f5 (_nonMetricBin   meta) (_nonMetricBin   lhs) (_nonMetricBin   rhs)
      , _dynamicBin     = parZipWith3 rpar f6 (_dynamicBin     meta) (_dynamicBin     lhs) (_dynamicBin     rhs)
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
      <$> (fmap toMissing . continuousCharacterBins)
      <*> (fmap toMissing . nonAdditiveCharacterBins)
      <*> (fmap toMissing . additiveCharacterBins)
      <*> (fmap toMissing . metricCharacterBins)
      <*> (fmap toMissing . nonMetricCharacterBins)
      <*> (fmap toMissing . dynamicCharacters)
    )
