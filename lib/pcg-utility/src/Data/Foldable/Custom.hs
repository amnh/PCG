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
-- This adds strict version of foldMap.
-- Note: This is due to be added to Foldable in base 4.13.
--
-----------------------------------------------------------------------------

module Data.Foldable.Custom where

import Data.Foldable (Foldable (foldl'))

-- | Peforms a foldMap that is strict in the accumulator.
--
foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ acc a -> acc <> f a) mempty
