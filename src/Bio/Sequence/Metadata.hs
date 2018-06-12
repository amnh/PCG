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

{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}

module Bio.Sequence.Metadata
  ( MetadataBlock()
  , MetadataSequence()
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
  ) where


import qualified Bio.Sequence.Block      as Blk
import           Bio.Sequence.Block.Internal    (Block(..))
import           Bio.Sequence.Block.Metadata
import           Control.DeepSeq
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.DList              hiding (foldr,toList)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty             (NonEmpty)
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.Vector.NonEmpty           (Vector)
import qualified Data.Vector.NonEmpty    as V
import           GHC.Generics
import           Text.XML


-- |
-- A multi-level partitioned, non-empty sequence of column metadata.
--
-- A sequence is partitioned into blocks, each block contains block-level metadata
-- and also seperate metadata for each column in the block.
--
-- Blocks are optimized atomically with resepect to network resolutions.
newtype MetadataSequence m e d
    = MetaSeq (Vector (MetadataBlock m e d))
    deriving (Generic)


type instance Element (MetadataSequence m e d) = MetadataBlock m e d


{-
instance Bifunctor (MetadataSequence m) where

    bimap f g = fromBlocks . fmap (bimap f g) . toBlocks

    first f   = fromBlocks . fmap (first f  ) . toBlocks

    second = fmap


instance Functor (MetadataSequence m e) where

    fmap f = fromBlocks . fmap (fmap f) . toBlocks

    (<$) v = fromBlocks . fmap (v <$) . toBlocks
-}


instance MonoFoldable (MetadataSequence m e d) where

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


instance MonoFunctor (MetadataSequence m e d) where

    {-# INLINE omap #-}
    omap f = fromBlocks . fmap f . toBlocks


instance MonoTraversable (MetadataSequence m e d) where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBlocks . traverse f . toBlocks

    {-# INLINE omapM #-}
    omapM = otraverse


instance (NFData m, NFData e, NFData d) => NFData (MetadataSequence m e d)


{-
-- | (✔)
instance ( Show m
         , Show e
         , Show d
         ) => Show (MetadataSequence m e d) where

    show = foldMapWithKey f . toBlocks
      where
        f blockNumber shownBlock = mconcat
            [ "Metadata Block #"
            , show blockNumber
            , "\n\n"
            , indent (show shownBlock)
            , "\n"
            ]
        indent = unlines . fmap ("  "<>) . lines


-- | (✔)
instance ( ToXML m
         , ToXML e
         , ToXML d
         ) => ToXML (MetadataSequence m e d) where

    toXML = collapseElemList "Metadata_sequence" [] . toBlocks
-}


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE toBlocks #-}
toBlocks :: MetadataSequence m e d -> NonEmpty (MetadataBlock m e d)
toBlocks (MetaSeq x) = toNonEmpty x


-- |
-- Constructs a 'MetadataSequence' from a non-empty colection of blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: NonEmpty (MetadataBlock m e d) -> MetadataSequence m e d
fromBlocks = MetaSeq . V.fromNonEmpty


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE toBlockVector #-}
toBlockVector :: MetadataSequence m e d -> Vector (MetadataBlock m e d)
toBlockVector (MetaSeq x) =  x


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE fromBlockVector #-}
fromBlockVector :: Vector (MetadataBlock m e d) -> MetadataSequence m e d
fromBlockVector = MetaSeq


defaultUnaryMetadataSequence :: MetadataSequence () e d
defaultUnaryMetadataSequence = fromBlocks . pure . MB $ Block
    { blockMetadata   = ()
    , continuousBins  = mempty
    , nonAdditiveBins = mempty
    , additiveBins    = mempty
    , metricBins      = mempty
    , nonMetricBins   = mempty
    , dynamicBins     = mempty
    }
