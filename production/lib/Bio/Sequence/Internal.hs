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

{-# LANGUAGE TypeFamilies #-}

--TODO: Add instance of Functor 
--TODO: Add instance of BiFunctor 

module Bio.Sequence.Internal
  ( CharacterSequence()
  , toBlocks
  , fromBlocks
  ) where

--import           Bio.Character.Encodable
import           Bio.Sequence.Block   (CharacterBlock)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Monoid
import           Data.MonoTraversable


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


instance ( Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (CharacterSequence m i c f a d) where

    show = foldMapWithKey f . toBlocks
      where
        f blockNumber shownBlock = mconcat
            [ "Character Block #"
            , show blockNumber
            , "\n\n"
            , indent (show shownBlock)
            , "\n"
            ]
        indent = unlines . fmap ("  "<>) . lines
    

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
