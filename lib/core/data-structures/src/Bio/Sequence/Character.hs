-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Character
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Bio.Sequence.Character
  ( CharacterSequence()
  , HasBlockCost
  , fromNonEmpty
  ) where

import           Bio.Sequence.Block      (CharacterBlock, HasBlockCost)
import           Bio.Sequence.Internal
import           Control.DeepSeq
import           Control.Lens
import           Data.Bifunctor
import           Data.Binary
import           Data.Foldable
import           Data.Key
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty    (Vector)
import qualified Data.Vector.NonEmpty    as V
import           GHC.Generics
import           Text.XML
import           TextShow                (TextShow)
import           TextShow.Instances      ()

-- |
-- A multi-level partitioned, non-empty sequence of characters.
--
-- A sequence is partitioned into blocks, each block is partitioned into static
-- and dynamic characters, with the static characters partitioned by optimization
-- routines.
--
-- Blocks are optimized atomically with respect to network resolutions.
--
-- Some blocks may not exist on a given node after performing a pre-order traversal.
-- This occurs when the minimal display tree for a given block does not include
-- all the nodes in the DAG. In this case the nodes not included in the topology
-- will have "missing" final assignments for all blocks for which that topology
-- was minimal.
newtype CharacterSequence u v w x y z
    = CharSeq (Vector (CharacterBlock u v w x y z))
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype  (Binary, TextShow)


type instance Element (CharacterSequence u v w x y z) = CharacterBlock u v w x y z


instance Bifunctor (CharacterSequence u v w x) where

    bimap f g = fromBlocks . fmap (bimap f g) . toBlocks

    first f   = fromBlocks . fmap (first f  ) . toBlocks

    second = fmap


instance Functor (CharacterSequence u v w x y) where

    fmap f = fromBlocks . fmap (fmap f) . toBlocks

    (<$) v = fromBlocks . fmap (v <$) . toBlocks


instance HasBlocks
  (CharacterSequence u  v  w  x  y  z )
  (CharacterSequence u' v' w' x' y' z')
  (Vector (CharacterBlock u  v  w  x  y  z ))
  (Vector (CharacterBlock u' v' w' x' y' z')) where

      blockSequence = iso toBlocks fromBlocks

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
         ) => Show (CharacterSequence u v w x y z) where

    show seek = {- prefix <> "\n" <> -} suffix
      where
--        prefix = "Sequence Cost: " <> show (sequenceCost seek)
        suffix = foldMapWithKey f $ toBlocks seek
        f blockNumber shownBlock = fold
            [ "Character Block #"
            , show blockNumber
            , "\n\n"
            , indent (show shownBlock)
            , "\n"
            ]
        indent = unlines . fmap ("  "<>) . lines


instance ( ToXML u
         , ToXML v
         , ToXML w
         , ToXML y
         , ToXML z
         ) => ToXML (CharacterSequence u v w x y z) where

    toXML = collapseElemList "Character_sequence" [] . toBlocks


-- |
-- /O(n)/
--
-- Construct a 'CharacterSequence' from a non-empty structure of character blocks.
{-# INLINE fromNonEmpty #-}
fromNonEmpty
  :: Foldable1 f
  => f (CharacterBlock u v w x y z)
  -> CharacterSequence u v w x y z
fromNonEmpty = CharSeq . V.fromNonEmpty


{-# INLINE fromBlocks #-}
fromBlocks :: Vector (CharacterBlock u v w x y z) -> CharacterSequence u v w x y z
fromBlocks = CharSeq


{-# INLINE toBlocks #-}
toBlocks :: CharacterSequence u v w x y z ->  Vector (CharacterBlock u v w x y z)
toBlocks (CharSeq x) = x
