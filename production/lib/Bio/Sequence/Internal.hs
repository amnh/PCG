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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- This is so that the Show instance compiles, even though the "real valued"
-- cost variable 'r' doesn't appear on the right hand side of the double arrow.
{-# LANGUAGE UndecidableInstances #-}

--TODO: Add instance of Functor 
--TODO: Add instance of BiFunctor 

module Bio.Sequence.Internal
  ( CharacterSequence()
  , HasBlockCost
  , toBlocks
  , fromBlocks
  , hexmap
  , hexTranspose
  , hexZipWith
  , sequenceCost
  ) where


--import           Bio.Character.Decoration.Continuous
import           Bio.Sequence.Block             (CharacterBlock, HasBlockCost)
import qualified Bio.Sequence.Block      as Blk
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty             (NonEmpty)
import qualified Data.List.NonEmpty      as NE    
import           Data.Monoid
import           Data.MonoTraversable
--import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Prelude                 hiding (zipWith)


-- |
-- A multi-level partitioned, non-empty sequence of characters.
--
-- A sequence is partitioned into blocks, each block is partitioned into static
-- and dynamic charcters, with the static characters partitioned by optimization
-- routines.
--
-- Blocks are optimized atomically with resepect to network resolutions.
newtype CharacterSequence u v w x y z
    = CharSeq (NonEmpty (CharacterBlock u v w x y z))
    deriving (Eq)


type instance Element (CharacterSequence u v w x y z) = CharacterBlock u v w x y z


instance MonoFunctor (CharacterSequence u v w x y z) where

    {-# INLINE omap #-}
    omap f = fromBlocks . fmap f . toBlocks


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


-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable (CharacterSequence u v w x y z) where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBlocks . traverse f . toBlocks

    {-# INLINE omapM #-}
    omapM = otraverse


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


-- |
-- Performs a 2D transform on the 'Traversable' structure of 'CharacterSequence'
-- values.
--
-- Assumes that the 'CharacterSequence' values in the 'Traversable' structure are
-- of equal length. If this assumtion is violated, the result will be truncated. 
hexTranspose :: Traversable1 t => t (CharacterSequence u v w x y z) -> CharacterSequence [u] [v] [w] [x] [y] [z]
hexTranspose = fromBlocks . deepTranspose . fmap toBlocks . toList
  where
--    deepTranspose :: [(NonEmpty (CharacterBlock m i c f a d))] -> NonEmpty (CharacterBlock (t m) (t i) (t c) (t f) (t a) (t d))
    deepTranspose val =
      let beta = NE.unfoldr f val -- :: NonEmpty [CharacterBlock m i c f a d]
        in  fmap Blk.hexTranspose beta
      where
--        f :: [NonEmpty (CharacterBlock m i c f a d)] -> ([CharacterBlock m i c f a d], Maybe [NonEmpty (CharacterBlock m i c f a d)])
        f = second sequenceA . unzip . fmap NE.uncons


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
toBlocks (CharSeq x) = x


-- |
-- Constructs a 'CharacterSequence' from a non-empty colection of blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: NonEmpty (CharacterBlock u v w x y z) -> CharacterSequence u v w x y z
fromBlocks = CharSeq


-- |
-- Calculates the cost of a 'CharacterSequence'. Performs some of the operation
-- in parallel.
sequenceCost :: HasBlockCost u v w x y z i r => CharacterSequence u v w x y z -> r
sequenceCost = sum . parmap rpar Blk.blockCost . toBlocks
