{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module Data.Graph.Sequence.Class where

import Control.Lens
import Control.Lens.Tuple
import Data.Kind
import Data.Vector
import qualified Data.Vector as Vector
import Data.Coerce
import           Control.Parallel.Custom
import           Control.Parallel.Strategies

class MetricSpace charSeq where
  dist :: charSeq -> charSeq -> Double

class MedianSpace charSeq where
  median :: charSeq -> charSeq -> charSeq

-- |
-- A 'Lens' for the 'characterSequence' field
class HasCharacterSequence s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _characterSequence :: Lens s t a b


-- |
-- A 'Lens' for the 'characterSequence' field
class HasCharacterSequence' s a | s -> a where
    _characterSequence' :: Lens' s a


-- |
-- A 'Iso' for 'blockSequence'.
class HasBlocks s t a b | s t -> a, s t  -> b where
    blockSequence :: Iso s t a b

instance HasBlocks
  (MetadataSequence block m)
  (MetadataSequence block m')
  (Vector (MetadataBlock block m))
  (Vector (MetadataBlock block m')) where

    blockSequence = iso coerce coerce

instance HasBlocks
  (CharacterSequence block)
  (CharacterSequence block')
  (Vector block)
  (Vector block') where

      blockSequence = iso coerce coerce

newtype CharacterSequence block = CharacterSequence {getCharacterSequence :: Vector block}


characterLeafInitialise :: (BlockBin block)
  => MetadataSequence  block meta
  -> CharacterSequence (LeafBin block)
  -> CharacterSequence block
characterLeafInitialise meta charSeq =
  let
    mSeq = blockDataSet <$> view blockSequence meta
    cSeq = charSeq ^. blockSequence
  in
    view (from blockSequence) $ Vector.zipWith leafInitialise mSeq cSeq


characterBinaryPostorder :: (BlockBin block)
  => MetadataSequence block meta
  -> CharacterSequence block
  -> CharacterSequence block
  -> CharacterSequence block
characterBinaryPostorder meta leftCharSeq rightCharSeq =
  let
    mSeq      = blockDataSet <$> view blockSequence meta
    leftCSeq  = leftCharSeq  ^. blockSequence
    rightCSeq = rightCharSeq ^. blockSequence  
  in
    view (from blockSequence) $ Vector.zipWith3 binaryPostorder mSeq leftCSeq rightCSeq



data MetadataBlock block meta = MetadataBlock
    { blockMetadata :: meta
    , blockDataSet  :: CharacterMetadata block
    }
--    deriving stock   (Generic, Show)
--    deriving anyclass(NFData)
newtype MetadataSequence block meta = MetadataSequence
  { getMetadataSequence :: Vector (MetadataBlock block meta)
  }

class BlockBin bin where
  type LeafBin           bin
  -- to do: make this a data family with newtypes to make it injective for the lens in subblock
  type CharacterMetadata bin

  leafInitialise :: CharacterMetadata bin -> LeafBin bin -> bin
  binaryPostorder  :: CharacterMetadata bin -> bin -> bin -> bin

class (BlockBin subBlock, BlockBin block) => SubBlock subBlock block where
  _hasSubBlock :: Lens' block subBlock

  _hasSubBlockMetadata :: Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  
  

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

   -- Note we can add parallelism here
  leafInitialise ~(m1, m2, m3, m4, m5, m6) ~(b1, b2, b3, b4, b5, b6) =
    ( leafInitialise m1 b1
    , leafInitialise m2 b2
    , leafInitialise m3 b3
    , leafInitialise m4 b4
    , leafInitialise m5 b5
    , leafInitialise m6 b6
    )
    
   -- Note we can add parallelism here
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




class AssociatedCharacterMetadata charMeta charType | charType -> charMeta where


-- |
-- A 'Lens' for the 'characterCost' field.
class HasCharacterCost s a | s -> a where

    {-# MINIMAL characterCost #-}
    characterCost :: Lens' s a

--type HasBlockCost block =
--    ( HasCharacterCost u Double
--    , HasCharacterCost v Word
--    , HasCharacterCost w Word
--    , HasCharacterCost x Word
--    , HasCharacterCost y Word
--    , HasCharacterCost z Word
--    )

class HasBlockCost block where

class HasSequenceCost block where
  sequenceCost
    :: meta
    -> charSeq block
    -> Double
  



type family FinalDecoration   a :: Type
--type family CharacterSequence a :: Type

{-
class HexZippableMeta (charSeq :: Type -> Type -> Type -> Type -> Type -> Type -> Type) where
  hexZipMeta
     :: ( AssociatedCharacterMetadata continuousMeta   u
        , AssociatedCharacterMetadata discreteMeta     v
        , AssociatedCharacterMetadata discreteMeta'    w
        , AssociatedCharacterMetadata discreteWithTCM  x
        , AssociatedCharacterMetadata discreteWithTCM' y
        , AssociatedCharacterMetadata dynamicMeta      z
        , MetadataSequence            meta
        )
     => (continuousMeta   -> u -> u')
     -> (discreteMeta     -> v -> v')
     -> (discreteMeta'    -> w -> w')
     -> (discreteWithTCM  -> x -> x')
     -> (discreteWithTCM' -> y -> y')
     -> (dynamicMeta      -> z -> z')
     -> meta
     -> charSeq u   v   w   x   y   z
     -> charSeq u'  v'  w'  x'  y'  z'



  hexZipMeta2
   :: ( AssociatedCharacterMetadata continuousMeta   u
      , AssociatedCharacterMetadata discreteMeta     v
      , AssociatedCharacterMetadata discreteMeta'    w
      , AssociatedCharacterMetadata discreteWithTCM  x
      , AssociatedCharacterMetadata discreteWithTCM' y
      , AssociatedCharacterMetadata dynamicMeta      z
      , MetadataSequence            meta
      )
   => (continuousMeta   -> u -> u  -> u')
   -> (discreteMeta     -> v -> v  -> v')
   -> (discreteMeta'    -> w -> w  -> w')
   -> (discreteWithTCM  -> x -> x  -> x')
   -> (discreteWithTCM' -> y -> y  -> y')
   -> (dynamicMeta      -> z -> z  -> z')
   -> meta
   -> charSeq u   v   w   x   y   z
   -> charSeq u   v   w   x   y   z
   -> charSeq u'  v'  w'  x'  y'  z'
-}
