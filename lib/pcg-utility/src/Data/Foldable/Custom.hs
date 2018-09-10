{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for collecting 'Left' values of 'Either' forming a 'Validation'
-- similar context.
--
-----------------------------------------------------------------------------

module Data.Foldable.Custom where

import Data.Foldable (Foldable(foldl'))

-- | Peforms a foldMap that is strict in the accumulator.
--
foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ !acc a -> acc <> f a) mempty
