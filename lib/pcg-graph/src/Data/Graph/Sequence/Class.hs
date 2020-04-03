{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs          #-}

module Data.Graph.Sequence.Class where

import Control.Lens
import Control.Lens.Tuple
import Data.Kind
import Data.Vector
import qualified Data.Vector as Vector
import Data.Coerce
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import Data.Kind (Type)
import GHC.Generics (Generic)


-- |
-- A type class for those types which have a metric.
class MetricSpace charSeq where
  dist :: charSeq -> charSeq -> Double

-- |
-- A typeclass for those types which have a function for finding medians.
class MedianSpace charSeq where
  median :: charSeq -> charSeq -> charSeq


-- |
-- The BlockBin class gives an abstraction over a collection of character bins.
-- For example we would expect a particular character (Continuous, Discrete, Dynamic etc.)
-- to be an instance of this class but also that records containing fields of characters
-- to also be instances. In the case of records we expect the associated types and functions
-- to operate component wise (for example one would expect:
--
-- LeafBin (bin1, bin2) ~ (LeafBin bin1, LeafBin bin2)
class BlockBin bin where
-- For any given character we have an associated type for how that character
-- is intialised at a leaf.
  type LeafBin           bin
-- Associated with each character type we have specific character metadata.
-- For example here we might store something like the character metric.
  type CharacterMetadata bin

-- This function takes the associated metadata and the data initially on a leaf
-- and `decorates` that data so that it can be used in a full postorder traversal
  leafInitialise :: CharacterMetadata bin -> LeafBin bin -> bin
-- The binary postPostorder function takes the metadata and the data at two internal
-- nodes and gives the character we get back at the parent node.
  binaryPostorder  :: CharacterMetadata bin -> bin -> bin -> bin


-- Note (TODO) :
--   - Currently we write a baked in instance for a tuple of size 6.
--   - Eventually we instead plan to use something like generics-sop to generate
--   - all such instance which are products of primitive character types. This
--   - will include records and thus allow us to have strict or unpacked fields
--   - for those characters for which this is appropriate.
instance (BlockBin a, BlockBin b, BlockBin c, BlockBin d, BlockBin e, BlockBin f)
    => BlockBin (a, b, c, d, e, f) where
  type LeafBin (a, b, c, d, e, f)
    = ( LeafBin a
      , LeafBin b
      , LeafBin c
      , LeafBin d
      , LeafBin e
      , LeafBin f
      )
  type CharacterMetadata (a, b, c, d, e, f)
    = ( CharacterMetadata a
      , CharacterMetadata b
      , CharacterMetadata c
      , CharacterMetadata d
      , CharacterMetadata e
      , CharacterMetadata f
      )

   -- Note (TODO): we can add parallelism here
  leafInitialise ~(m1, m2, m3, m4, m5, m6) ~(b1, b2, b3, b4, b5, b6) =
    ( leafInitialise m1 b1
    , leafInitialise m2 b2
    , leafInitialise m3 b3
    , leafInitialise m4 b4
    , leafInitialise m5 b5
    , leafInitialise m6 b6
    )

   -- Note (TODO): we can add parallelism here
  binaryPostorder
    ~(m1, m2, m3, m4, m5, m6)
    ~(b1, b2, b3, b4, b5, b6)
    ~(b1', b2', b3', b4', b5', b6')
    = ( binaryPostorder m1 b1 b1'
      , binaryPostorder m2 b2 b2'
      , binaryPostorder m3 b3 b3'
      , binaryPostorder m4 b4 b4'
      , binaryPostorder m5 b5 b5'
      , binaryPostorder m6 b6 b6'
      )

-- |
-- A character sequence is backed by an underlying vector of blocks.
-- The block type is expected to be an instance of the `BlockBin` typeclass.
newtype CharacterSequence block = CharacterSequence {getCharacterSequence :: Vector block}
  deriving stock Functor


-- |
-- A bidirectional 'Lens' for a type containing a 'characterSequence' field.
class HasCharacterSequence s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _characterSequence :: Lens s t a b


-- |
-- A unidirectional 'Lens'' for a type containing a 'characterSequence' field
class HasCharacterSequence' s a | s -> a where
    _characterSequence' :: Lens' s a


-- |
-- A metadata block has a blockMetadtata field containing global metadtata for
-- an entire character block. It also contains a field holding `CharacterMetadata`
-- for the metadata associated to specific characters.
--
-- Note: CharacterMetadata is an associated type family from the BlockBin typeclass
--       storing metadata associated with a specific character type.
data MetadataBlock block meta = MetadataBlock
    { blockMetadata :: meta
    , binMetadata   :: CharacterMetadata block
    }
  deriving stock Functor

-- |
-- A 'Lens'` the `blockMetadata` field in a `MetadataBlock`.
class HasBlockMetadata s a | s -> a where
  _blockMetadata :: Lens' s a


instance HasBlockMetadata (MetadataBlock block meta) meta where
   {-# INLINE _blockMetadata #-}
   _blockMetadata = lens blockMetadata $ \e x -> e {blockMetadata = x}


-- |
-- A `Lens'` for the `binMetadata` field in a `MetadataBlock`.
--
-- Note: This is a top-level binding as opposed to a typeclass as this
-- field contains an type family which Haskell does not allow to appear
-- in an instance. The reason here is that if this did not have
-- a functional dependency then because the type family is not necessarily
-- injective this would lead to overlapping instances and so instance
-- resolution would not be type directed.
_binMetadata
  :: Lens
       (MetadataBlock block meta)
       (MetadataBlock block' meta)
       (CharacterMetadata block)
       (CharacterMetadata block')
_binMetadata = lens binMetadata $ \e x -> e {binMetadata = x}


-- |
-- A MetadataSequence is a `Vector` of `Metadata` blocks.
newtype MetadataSequence block meta = MetadataSequence
  { getMetadataSequence :: Vector (MetadataBlock block meta)
  }
  deriving stock Functor


-- |
-- A class for those types which have a lens onto a `MetadataSequence`.
class HasMetadataSequence s a | s -> a where
  _metadataSequence :: Lens' s a

-- |
-- A 'Iso' for any sort of 'blockSequence'.
class HasBlocks s a | s -> a where
  _blockSequence :: Iso' s a

-- |
-- The underlying representation of a `MetadataSequence` is a `Vector` of metadata blocks.
instance HasBlocks (MetadataSequence block m) (Vector (MetadataBlock block m)) where

  _blockSequence = iso coerce coerce

-- |
-- The underlying representation of a `CharacterSequence` is a `Vector` of blocks
-- which implement the `BlockBin` typeclass.
instance HasBlocks (CharacterSequence block) (Vector block) where

  _blockSequence = iso coerce coerce


-- |
-- If we have a `CharacterSequence` then this function initialises
-- the leaves over all blocks.
characterLeafInitialise :: (BlockBin block)
  => MetadataSequence  block meta
  -> CharacterSequence (LeafBin block)
  -> CharacterSequence block
characterLeafInitialise meta charSeq =
  let
    mSeq = binMetadata <$> view _blockSequence  meta
    cSeq = charSeq ^. _blockSequence
  in
    view (from _blockSequence) $ Vector.zipWith leafInitialise mSeq cSeq

-- |
-- If we have a pair of `CharacterSequence`s then this function performs
-- the blockwise postorder update.
characterBinaryPostorder :: (BlockBin block)
  => MetadataSequence block meta
  -> CharacterSequence block
  -> CharacterSequence block
  -> CharacterSequence block
characterBinaryPostorder meta leftCharSeq rightCharSeq =
  let
    mSeq      = binMetadata <$> view _blockSequence meta
    leftCSeq  = leftCharSeq  ^. _blockSequence
    rightCSeq = rightCharSeq ^. _blockSequence
  in
    view (from _blockSequence) $ Vector.zipWith3 binaryPostorder mSeq leftCSeq rightCSeq


-- |
-- A typeclass to indicate blocks which have a associated cost functions.
class (BlockBin block) => HasBlockCost block where
  staticCost  :: MetadataBlock block m -> block -> Double
  dynamicCost :: MetadataBlock block m -> block -> Double
  rootcost    :: MetadataBlock block m -> block -> Double
  blockCost   :: MetadataBlock block m -> block -> Double


class HasCharacterCost char cost | char -> cost where
  _characterCost :: Lens' char cost

class HasCharacterWeight charMeta cost | charMeta -> cost where
  _characterWeight :: Lens' charMeta cost

class HasTraversalFoci dynMeta traversalFoci | dynMeta -> traversalFoci where
  _traversalFoci :: Lens' dynMeta traversalFoci

type DynCharacterSubBlock subBlock dynChar =
  ( subBlock ~ Vector dynChar
  , CharacterMetadata subBlock ~ Vector (CharacterMetadata dynChar)
  )



-- Note (TODO): does it make sense to have this as a typeclass or should it be a function
-- using blockcost above?
class HasSequenceCost block where
  sequenceCost :: MetadataSequence block meta -> CharacterSequence block -> Double



-- Note (TODO): this should probably be rolled into the BlockBin typeclass
type family FinalDecoration a :: Type
