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
-- This adds strict versions of some functions from Foldable and some
-- strict helper functions.
-- Note: FoldMap' is due to be added to Foldable in base 4.13.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Foldable.Custom
  ( foldl''
  , foldMap'
  , sum'
  , minimum'
  , maximum'
  , maximumBy'
  , minimumBy'
  )
  where

import Control.DeepSeq (NFData, ($!!))
import Data.Coerce     (Coercible, coerce)
import Data.Foldable   (Foldable (foldl', foldr))
import Data.Maybe      (fromMaybe)
import Data.Monoid     (Sum (..))


-- |
-- Performs an even stricter foldl reducing the accumulator
-- to normal form as opposed to weak normal form using NFData.
foldl'' :: (Foldable t, NFData b) => (b -> a -> b) -> b -> t a -> b
foldl'' f z0 xs = foldr f' id xs z0
  where
    f' x k z = k $!! f z x


-- |
-- Peforms a foldMap that is strict in the accumulator.
{-# INLINE foldMap' #-}
foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ acc a -> acc <> f a) mempty


foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' f xs = fromMaybe
                (errorWithoutStackTrace "foldl1: empty structure")
                (foldl' mf Nothing xs)
  where
    mf m y = Just (case m of
                     Nothing -> y
                     Just x  -> f x y)


-- |
-- Performs a sum that is strict in the accumulator.
{-# INLINE sum' #-}
sum' :: (Num a, Foldable t) => t a -> a
sum' = getSum #. foldMap' Sum


-- |
-- Performs a minimum that is strict in the accumulator.
{-# INLINE minimum' #-}
minimum' :: forall a t . (Ord a, Foldable t) => t a -> a
minimum' = fromMaybe (errorWithoutStackTrace "minimum: empty structure")
         . getMin . foldMap' (Min #. (Just :: a -> Maybe a))


-- |
-- Performs a maximum that is strict in the accumulator.
{-# INLINE maximum' #-}
maximum' :: forall a t. (Ord a, Foldable t) => t a -> a
maximum' = fromMaybe (errorWithoutStackTrace "maximum: empty structure")
         . getMax . foldMap' (Max #. (Just :: a -> Maybe a))


-- |
-- Strictly folds over the structure and returns the minimum value.
{-# INLINE minimumBy' #-}
minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy' cmp = foldl1' min'
  where
    min' x y = case cmp x y of
                 GT -> y
                 _  -> x


-- |
-- Strictly folds over the structure and returns the maximim value.
{-# INLINE maximumBy' #-}
maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy' cmp = foldl1' max'
  where
    max' x y = case cmp x y of
                 GT -> x
                 _  -> y


-- This is from Data.Functor.Utils but is internal to base (as explained there),
-- so we reproduce it here.
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


-- |
-- There is an explanation in 'Data.Coerce' for why this is needed.
{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
