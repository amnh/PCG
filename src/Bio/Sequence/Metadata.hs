-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded characters
-- Coded characters are dynamic characters recoded as
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Bio.Sequence.Metadata
  ( MetadataBlock()
  , MetadataSequence()
  , getBlockMetadata
  , getDynamicMetadata
  -- * Block Singletons
  , continuousToMetadataBlock
  , discreteToMetadataBlock
  , dynamicToMetadataBlock
  -- * Construction / Decomposition
  , toBlocks
  , fromBlocks
  , toBlockVector
  , fromBlockVector
  , defaultUnaryMetadataSequence
  -- * Mutation
  , setAllFoci
  , setFoci
  ) where


import           Bio.Sequence.Block.Internal (Block (..))
import           Bio.Sequence.Block.Metadata
import           Control.DeepSeq
import           Data.Foldable
import           Data.List.NonEmpty          (NonEmpty)
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty        (Vector)
import qualified Data.Vector.NonEmpty        as V
import           GHC.Generics
import           Text.XML


-- |
-- A multi-level partitioned, non-empty sequence of column metadata.
--
-- A sequence is partitioned into blocks, each block contains block-level metadata
-- and also seperate metadata for each column in the block.
--
-- Blocks are optimized atomically with resepect to network resolutions.
newtype MetadataSequence m
    = MetaSeq (Vector (MetadataBlock m))
    deriving (Generic)


type instance Element (MetadataSequence m) = MetadataBlock m


instance Functor MetadataSequence where

    fmap f = fromBlocks . fmap (fmap f) . toBlocks

    (<$) v = fromBlocks . fmap (v <$) . toBlocks


instance MonoFoldable (MetadataSequence m) where

    {-# INLINE ofoldMap #-}
    ofoldMap f = foldMap f . toBlocks

    {-# INLINE ofoldr #-}
    ofoldr f e = foldr f e . toBlocks

    {-# INLINE ofoldl' #-}
    ofoldl' f e = foldl' f e . toBlocks

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = foldr1 f . toBlocks

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = foldl1 f . toBlocks

    {-# INLINE onull #-}
    onull = const False

    {-# INLINE olength #-}
    olength = length . toBlocks


instance MonoFunctor (MetadataSequence m) where

    {-# INLINE omap #-}
    omap f = fromBlocks . fmap f . toBlocks


instance MonoTraversable (MetadataSequence m) where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBlocks . traverse f . toBlocks

    {-# INLINE omapM #-}
    omapM = otraverse


instance (NFData m) => NFData (MetadataSequence m)


-- | (✔)
instance ToXML (MetadataSequence m) where

    toXML = collapseElemList "Metadata_sequence" [] . toBlocks


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE toBlocks #-}
toBlocks :: MetadataSequence m -> NonEmpty (MetadataBlock m)
toBlocks (MetaSeq x) = toNonEmpty x


-- |
-- Constructs a 'MetadataSequence' from a non-empty colection of blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: NonEmpty (MetadataBlock m) -> MetadataSequence m
fromBlocks = MetaSeq . V.fromNonEmpty


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE toBlockVector #-}
toBlockVector :: MetadataSequence m -> Vector (MetadataBlock m)
toBlockVector (MetaSeq x) =  x


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE fromBlockVector #-}
fromBlockVector :: Vector (MetadataBlock m) -> MetadataSequence m
fromBlockVector = MetaSeq


defaultUnaryMetadataSequence :: MetadataSequence ()
defaultUnaryMetadataSequence = fromBlocks . pure . MB $ Block
    { blockMetadata   = ()
    , continuousBins  = mempty
    , nonAdditiveBins = mempty
    , additiveBins    = mempty
    , metricBins      = mempty
    , nonMetricBins   = mempty
    , dynamicBins     = mempty
    }
