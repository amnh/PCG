{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Custom
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This adds strict versions of some functions from Foldable.
-- Note: FoldMap' is due to be added to Foldable in base 4.13.
--
-----------------------------------------------------------------------------

module Data.Foldable.Custom
  ( foldMap'
  , sum'
  , minimum'
  , maximum'
  , maximumBy'
  , minimumBy'
  )
  where

import Data.Coerce   (Coercible, coerce)
import Data.Foldable (Foldable (foldl'))
import Data.Monoid   (Sum (..))
import Data.Maybe    (fromMaybe)

-- | Peforms a foldMap that is strict in the accumulator.
--
foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
{-# INLINE foldMap' #-}
foldMap' f = foldl' (\ acc a -> acc <> f a) mempty

-- TODO (CM): fix this.
foldl1' :: (Foldable t) => (a -> a -> a) -> t a -> a
foldl1' f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                (foldl' mf Nothing xs)
  where
    mf m y = Just (case m of
                     Nothing -> y
                     Just x  -> f x y)


-- | Performs a sum that is strict in the accumulator.
--
sum' :: (Num a, Foldable t) => t a -> a
{-# INLINE sum' #-}
sum' = getSum #. foldMap' Sum


-- | Performs a minimum that is strict in the accumulator.
--
minimum' :: forall a t . (Ord a, Foldable t) => t a -> a
{-# INLINE minimum' #-}
minimum' = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
  getMin . foldMap' (Min #. (Just :: a -> Maybe a))

-- | Performs a maximum that is strict in the accumulator.
maximum' :: forall a t. (Ord a, Foldable t) => t a -> a
{-# INLINE maximum' #-}
maximum' = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
  getMax . foldMap' (Max #. (Just :: a -> Maybe a))

minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy' cmp = foldl1' min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy' cmp = foldl1' max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y


-- This is from Data.Functor.Utils but is internal to base (as explained there).
newtype Max a = Max {getMax :: Maybe a}
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Semigroup (Max a) where
    {-# INLINE (<>) #-}
    m <> Max Nothing = m
    Max Nothing <> n = n
    (Max m@(Just x)) <> (Max n@(Just y))
      | x >= y    = Max m
      | otherwise = Max n

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing

instance Ord a => Semigroup (Min a) where
    {-# INLINE (<>) #-}
    m <> Min Nothing = m
    Min Nothing <> n = n
    (Min m@(Just x)) <> (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing

-- | There is an explanation in 'base' for why this is needed.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
