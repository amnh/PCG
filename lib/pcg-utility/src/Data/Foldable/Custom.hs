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
-- This adds strict version of some functions from Foldable.
-- Note: FoldMap' is due to be added to Foldable in base 4.13.
--
-----------------------------------------------------------------------------

module Data.Foldable.Custom where

import Data.Coerce   (Coercible, coerce)
import Data.Foldable (Foldable (foldl'))
import Data.Monoid   (Sum (..))

-- | Peforms a foldMap that is strict in the accumulator.
--
foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ acc a -> acc <> f a) mempty

-- | Performs a sum that is strict in the accumulator.
--
sum' :: (Num a, Foldable t) => t a -> a
sum' = getSum #. foldMap' Sum

-- | There is an explanation in 'base' for why this is needed.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
