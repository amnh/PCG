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

--TODO: Add instance of Functor 
--TODO: Add instance of BiFunctor 

module Bio.Sequence.Internal
  ( CharacterSequence()
  , toBlocks
  , fromBlocks
  , hexmap
  , hexTranspose
  , hexZipWith
  , sequenceCost
  ) where


import           Bio.Character.Decoration.Continuous
import           Bio.Sequence.Block               (CharacterBlock)
import qualified Bio.Sequence.Block      as Block
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty      as NE    
import           Data.Monoid
import           Data.MonoTraversable
--import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Prelude                 hiding   (zipWith)


-- |
-- A multi-level partitioned, non-empty sequence of characters.
--
-- A sequence is partitioned into blocks, each block is partitioned into static
-- and dynamic charcters, with the static characters partitioned by optimization
-- routines.
--
-- Blocks are optimized atomically with resepect to network resolutions.
newtype CharacterSequence m i c f a d
    = CharSeq (NonEmpty (CharacterBlock m i c f a d))
    deriving (Eq)


-- |
-- Perform a six way map over the polymorphic types.
hexmap :: (m -> m')
       -> (i -> i')
       -> (c -> c')
       -> (f -> f')
       -> (a -> a')
       -> (d -> d')
       -> CharacterSequence m  i  c  f  a  d
       -> CharacterSequence m' i' c' f' a' d'
hexmap f1 f2 f3 f4 f5 f6 = fromBlocks . parmap rpar (Block.hexmap f1 f2 f3 f4 f5 f6) . toBlocks


hexTranspose :: Traversable1 t => t (CharacterSequence m i c f a d) -> CharacterSequence [m] [i] [c] [f] [a] [d]
hexTranspose = fromBlocks . deepTranspose . fmap toBlocks . toList
  where
--    deepTranspose :: [(NonEmpty (CharacterBlock m i c f a d))] -> NonEmpty (CharacterBlock (t m) (t i) (t c) (t f) (t a) (t d))
    deepTranspose val =
        let beta = NE.unfold f val -- :: NonEmpty [CharacterBlock m i c f a d]
        in fmap Block.hexTranspose beta
      where
--        f :: [NonEmpty (CharacterBlock m i c f a d)] -> ([CharacterBlock m i c f a d], Maybe [NonEmpty (CharacterBlock m i c f a d)])
        f = second sequenceA . unzip . fmap NE.uncons
{-
        f (x@(_:|[]):xs) = (NE.head <$> (x:xs), Nothing)
        f            xs  = (NE.head <$>    xs , Just $ NE.tail <$> xs) 
-}

hexZipWith :: (m1 -> m2 -> m3)
           -> (i1 -> i2 -> i3)
           -> (c1 -> c2 -> c3)
           -> (f1 -> f2 -> f3)
           -> (a1 -> a2 -> a3)
           -> (d1 -> d2 -> d3)
           -> CharacterSequence m1 i1 c1 f1 a1 d1
           -> CharacterSequence m2 i2 c2 f2 a2 d2
           -> CharacterSequence m3 i3 c3 f3 a3 d3
hexZipWith f1 f2 f3 f4 f5 f6 lhs rhs = fromBlocks $ parZipWith rpar (Block.hexZipWith f1 f2 f3 f4 f5 f6) (toBlocks lhs) (toBlocks rhs)


-- |
-- Destructs a 'CharacterSequence' to it's composite blocks.
{-# INLINE toBlocks #-}
toBlocks :: CharacterSequence m i c f a d -> NonEmpty (CharacterBlock m i c f a d)
toBlocks (CharSeq x) = x


-- |
-- Constructs a 'CharacterSequence' from a non-empty colection of blocks.
{-# INLINE fromBlocks #-}
fromBlocks :: NonEmpty (CharacterBlock m i c f a d) -> CharacterSequence m i c f a d
fromBlocks = CharSeq


type instance Element (CharacterSequence m i c f a d) = CharacterBlock m i c f a d


instance MonoFunctor (CharacterSequence m i c f a d) where

    {-# INLINE omap #-}
    omap f = fromBlocks . fmap f . toBlocks


instance MonoFoldable (CharacterSequence m i c f a d) where

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
instance MonoTraversable (CharacterSequence m i c f a d) where

    {-# INLINE otraverse #-}
    otraverse f = fmap fromBlocks . traverse f . toBlocks

    {-# INLINE omapM #-}
    omapM = otraverse


instance ( Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         , HasCharacterCost   m Word
         , HasCharacterCost   i Word
         , HasCharacterCost   c Double
         , HasCharacterCost   f Word
         , HasCharacterCost   a Word
         , HasCharacterCost   d Word
         , HasCharacterWeight m Double
         , HasCharacterWeight i Double
         , HasCharacterWeight c Double
         , HasCharacterWeight f Double
         , HasCharacterWeight a Double
         , HasCharacterWeight d Double
         ) => Show (CharacterSequence m i c f a d) where

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


sequenceCost :: ( HasCharacterCost   m e
                , HasCharacterCost   i e
                , HasCharacterCost   c Double
                , HasCharacterCost   f e
                , HasCharacterCost   a e
                , HasCharacterCost   d e
                , HasCharacterWeight m Double
                , HasCharacterWeight i Double
                , HasCharacterWeight c Double
                , HasCharacterWeight f Double
                , HasCharacterWeight a Double
                , HasCharacterWeight d Double
                , Integral e
                )
             => CharacterSequence m i c f a d
             -> Double
sequenceCost = sum . parmap rpar Block.blockCost . toBlocks
