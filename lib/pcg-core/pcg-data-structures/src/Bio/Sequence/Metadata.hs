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
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Bio.Sequence.Metadata
  ( MetadataBlock()
  , MetadataSequence()
  -- * Block Singletons
  , continuousToMetadataBlock
  , discreteToMetadataBlock
  , dynamicToMetadataBlock
  -- * Construction / Decomposition
  , fromNonEmpty
  , unfoldr
  , hexFoldMap
  -- * Mutation
  , setAllFoci
  , setFoci
  , substituteMetadataSequence
  ) where


import           Bio.Character.Encodable
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence.Block.Metadata
import           Bio.Sequence.Internal
import           Control.DeepSeq
import Data.Key
import           Control.Lens
import           Data.Foldable
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty         (Vector)
import qualified Data.Vector.NonEmpty         as V
import           GHC.Generics
import           Text.XML
import Prelude hiding (zipWith)


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


instance HasBlocks (MetadataSequence m) (MetadataSequence m') (Vector (MetadataBlock m)) (Vector (MetadataBlock m')) where

    blockSequence = iso toBlocks fromBlocks


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
-- /O(n)/
--
-- Construct a 'MetadataSequence' from a non-empty structure of character blocks.
{-# INLINE fromNonEmpty #-}
fromNonEmpty
  :: Foldable1 f
  => f (MetadataBlock m)
  -> MetadataSequence m
fromNonEmpty = MetaSeq . V.fromNonEmpty


-- |
-- /O(n)/
--
-- Construct a 'MetadataSequence' by repeatedly applying the generator function
-- to a seed. The generator function always yields the next element and either
-- @ Just @ the new seed or 'Nothing' if there are no more elements to be
-- generated.
--
-- > unfoldr (\n -> (n, if n == 0 then Nothing else Just (n-1))) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
{-# INLINE unfoldr #-}
unfoldr
  :: (b -> (MetadataBlock m, Maybe b))
  -> b
  -> MetadataSequence m
unfoldr f = MetaSeq . V.unfoldr f


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE toBlocks #-}
toBlocks :: MetadataSequence m -> Vector (MetadataBlock m)
toBlocks (MetaSeq x) =  x


-- |
-- Destructs a 'MetadataSequence' to it's composite blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: Vector (MetadataBlock m) -> MetadataSequence m
fromBlocks = MetaSeq


hexFoldMap
  ::   forall m meta . (Monoid m)
  => (ContinuousCharacterMetadataDec                      -> m)
  -> (DiscreteCharacterMetadataDec                        -> m)
  -> (DiscreteCharacterMetadataDec                        -> m)
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> m)
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> m)
  -> (DynamicCharacterMetadataDec DynamicCharacterElement -> m)
  -> (MetadataSequence meta -> m)
hexFoldMap m1 m2 m3 m4 m5 m6 metaSeq =
    foldMap foldMapBlock (toBlocks metaSeq)
  where
    foldMapBlock :: MetadataBlock meta -> m
    foldMapBlock metaDataBlock =
        (foldMap m1 . (^.  continuousBin) $ metaDataBlock)
     <> (foldMap m2 . (^. nonAdditiveBin) $ metaDataBlock)
     <> (foldMap m3 . (^.    additiveBin) $ metaDataBlock)
     <> (foldMap m4 . (^.      metricBin) $ metaDataBlock)
     <> (foldMap m5 . (^.   nonMetricBin) $ metaDataBlock)
     <> (foldMap m6 . (^.     dynamicBin) $ metaDataBlock)



-- |
-- This function takes two metadata sequences and substitutes one into the other block
-- by block to match substituting a subgraph into a total graph.
substituteMetadataSequence
  :: Int  -- ^ Index of node we substitute into
  -> MetadataSequence m  -- ^ Metadata of subgraph
  -> MetadataSequence m  -- ^ Metadata of total graph
  -> MetadataSequence m
substituteMetadataSequence ind sub tot =
  let
    subBlocks = toBlocks sub
    totBlocks = toBlocks tot
  in
    fromBlocks (zipWith (substituteMetadataBlock ind) subBlocks totBlocks)
