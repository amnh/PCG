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

-- This is so that the Show instance compiles, even though the "real valued"
-- cost variable 'r' doesn't appear on the right hand side of the double arrow.
{-# LANGUAGE UndecidableInstances #-}

module Bio.Sequence.Internal
  ( CharacterSequence()
  , HasBlockCost
  -- * Construction / Decomposition
  , toBlocks
  , fromBlocks
  , toBlockVector
  , fromBlockVector
  -- * Other
  , hexmap
  , hexTranspose
  , hexZipWith
  , sequenceCost
  , sequenceRootCost
  ) where


--import           Bio.Character.Decoration.Continuous
import           Bio.Sequence.Block             (CharacterBlock, HasBlockCost, HasRootCost)
import qualified Bio.Sequence.Block      as Blk
import           Control.DeepSeq
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.DList              hiding (foldr,toList)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty             (NonEmpty)
--import qualified Data.List.NonEmpty      as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.Vector.NonEmpty           (Vector)
import qualified Data.Vector.NonEmpty    as V
import           GHC.Generics
import           Prelude                 hiding (zipWith)
import           Text.XML


-- |
-- A multi-level partitioned, non-empty sequence of characters.
--
-- A sequence is partitioned into blocks, each block is partitioned into static
-- and dynamic charcters, with the static characters partitioned by optimization
-- routines.
--
-- Blocks are optimized atomically with resepect to network resolutions.
--
-- Some blocks may not exist on a given node after performing a pre-order traversal.
-- This occurs when the minimal display tree for a given block does not include
-- all the nodes in the DAG. In this case the nodes not incldued in the topology
-- will have "missing" final assignments for all blocks for which that topology
-- was minimal.
newtype CharacterSequence u v w x y z
    = CharSeq (Vector (CharacterBlock u v w x y z))
    deriving (Eq, Generic)


type instance Element (CharacterSequence u v w x y z) = CharacterBlock u v w x y z


instance Bifunctor (CharacterSequence u v w x) where

    bimap f g = fromBlocks . fmap (bimap f g) . toBlocks

    first f   = fromBlocks . fmap (first f  ) . toBlocks

    second = fmap


instance Functor (CharacterSequence u v w x y) where

    fmap f = fromBlocks . fmap (fmap f) . toBlocks

    (<$) v = fromBlocks . fmap (v <$) . toBlocks


instance MonoFoldable (CharacterSequence u v w x y z) where

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


instance MonoFunctor (CharacterSequence u v w x y z) where

    {-# INLINE omap #-}
    omap f = fromBlocks . fmap f . toBlocks


-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable (CharacterSequence u v w x y z) where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBlocks . traverse f . toBlocks

    {-# INLINE omapM #-}
    omapM = otraverse


instance (NFData u, NFData v, NFData w, NFData x, NFData y, NFData z) => NFData (CharacterSequence u v w x y z)


-- | (✔)
instance ( Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         , Show r
         , HasBlockCost u v w x y z i r
         ) => Show (CharacterSequence u v w x y z) where

    show seek = prefix <> "\n" <> suffix
      where
        prefix = "Sequence Cost: " <> show (sequenceCost seek)
        suffix = foldMapWithKey f $ toBlocks seek
        f blockNumber shownBlock = mconcat
            [ "Character Block #"
            , show blockNumber
            , "\n\n"
            , indent (show shownBlock)
            , "\n"
            ]
        indent = unlines . fmap ("  "<>) . lines


-- | (✔)
instance ( ToXML u
         , ToXML v
         , ToXML w
--         , ToXML x
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterSequence u v w x y z) where

    toXML = collapseElemList "Character_sequence" [] . toBlocks


-- |
-- Perform a six way map over the polymorphic types.
hexmap :: (u -> u')
       -> (v -> v')
       -> (w -> w')
       -> (x -> x')
       -> (y -> y')
       -> (z -> z')
       -> CharacterSequence u  v  w  x  y  z
       -> CharacterSequence u' v' w' x' y' z'
hexmap f1 f2 f3 f4 f5 f6 = fromBlocks . parmap rpar (Blk.hexmap f1 f2 f3 f4 f5 f6) . toBlocks


-- TODO: Make sure the inner dimension's ordering is not reversed during the transpose.
--
-- |
-- Performs a 2D transform on the 'Traversable' structure of 'CharacterSequence'
-- values.
--
-- Assumes that the 'CharacterSequence' values in the 'Traversable' structure are
-- of equal length. If this assumtion is violated, the result will be truncated.
hexTranspose :: Traversable1 t => t (CharacterSequence u v w x y z) -> CharacterSequence [u] [v] [w] [x] [y] [z]
hexTranspose = toNList . invert . fmap toDList . toNonEmpty
  where
    toDList :: CharacterSequence u v w x y z -> CharacterSequence (DList u) (DList v) (DList w) (DList x) (DList y) (DList z)
    toDList = hexmap pure pure pure pure pure pure

    invert :: ( Foldable f
              , Semigroup u
              , Semigroup v
              , Semigroup w
              , Semigroup x
              , Semigroup y
              , Semigroup z
              )
           => f (CharacterSequence u v w x y z)
           -> CharacterSequence u v w x y z
    invert = foldr1 (hexZipWith (<>) (<>) (<>) (<>) (<>) (<>))

    toNList = hexmap toList toList toList toList toList toList


-- |
-- Performs a zip over the two character sequences. Uses the input functions to
-- zip the different character types in the character block.
--
-- Assumes that the 'CharacterSequence' values have the same number of character
-- blocks and the same number of each character type in the corresponding block
-- of each block. If this assumtion is violated, the result will be truncated.
hexZipWith :: (u -> u' -> u'')
           -> (v -> v' -> v'')
           -> (w -> w' -> w'')
           -> (x -> x' -> x'')
           -> (y -> y' -> y'')
           -> (z -> z' -> z'')
           -> CharacterSequence u   v   w   x   y   z
           -> CharacterSequence u'  v'  w'  x'  y'  z'
           -> CharacterSequence u'' v'' w'' x'' y'' z''
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs = fromBlocks $ parZipWith rpar (Blk.hexZipWith f1 f2 f3 f4 f5 f6) (toBlocks lhs) (toBlocks rhs)


-- |
-- Destructs a 'CharacterSequence' to it's composite blocks.
{-# INLINE toBlocks #-}
toBlocks :: CharacterSequence u v w x y z -> NonEmpty (CharacterBlock u v w x y z)
toBlocks (CharSeq x) = toNonEmpty x


-- |
-- Constructs a 'CharacterSequence' from a non-empty colection of blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: NonEmpty (CharacterBlock u v w x y z) -> CharacterSequence u v w x y z
fromBlocks = CharSeq . V.fromNonEmpty


-- |
-- Destructs a 'CharacterSequence' to it's composite blocks.
{-# INLINE toBlockVector #-}
toBlockVector :: CharacterSequence u v w x y z -> Vector (CharacterBlock u v w x y z)
toBlockVector (CharSeq x) =  x


-- |
-- Destructs a 'CharacterSequence' to it's composite blocks.
{-# INLINE fromBlockVector #-}
fromBlockVector :: Vector (CharacterBlock u v w x y z) -> CharacterSequence u v w x y z
fromBlockVector = CharSeq


-- |
-- Calculates the cumulative cost of a 'CharacterSequence'. Performs some of the
-- operation in parallel.
sequenceCost :: HasBlockCost u v w x y z i r => CharacterSequence u v w x y z -> r
sequenceCost = sum . parmap rpar Blk.blockCost . toBlocks


-- |
-- Calculates the root cost of a 'CharacterSequence'. Performs some of the
-- operation in parallel.
sequenceRootCost :: (HasRootCost u v w x y z r, Integral i) => i -> CharacterSequence u v w x y z -> r
sequenceRootCost rootCount = sum . parmap rpar (Blk.rootCost rootCount) . toBlocks
