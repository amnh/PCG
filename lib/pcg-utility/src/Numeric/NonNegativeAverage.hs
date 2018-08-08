-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.NonNegativeAverage
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}

module Numeric.NonNegativeAverage
  ( NonNegativeAverage()
  , fromNonNegativeAverage
  , fromNonNegativeValue
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Hashable
import           GHC.Generics
import           Test.QuickCheck


-- |
-- Defines an average of non-negative numbers.
--
-- This is a newtyped 'Ratio Word' for efficiency purposes.
--
-- Use 'fromNonNegativeValue' to create a single average value.
--
-- Use the `Semigroup` operator '(<>)' to combine two 'NonNegativeAverage' values.
--
-- Use 'fromNonNegativeAverage' when you are done accumulating values to average
-- to compute the /possibly/ non-integer average as a 'Fractional' value.
--
-- All instance operations are /O(1)/.
data NonNegativeAverage = Avg !Word !Word
  deriving (Data, Eq, Generic, Ord)


instance Arbitrary NonNegativeAverage where

    arbitrary = do
      num <- arbitrary
      den <- arbitrary
      pure . Avg num $ getPositive den


instance Bounded NonNegativeAverage where

    maxBound = Avg maxBound 1

    minBound = Avg 0 1


instance Hashable NonNegativeAverage where

    hashWithSalt salt (Avg d n) = hashWithSalt salt (d, n)


instance NFData NonNegativeAverage


instance Semigroup NonNegativeAverage where

    (Avg n d) <> (Avg n' d') = Avg (n + n') (d + d')


instance Show NonNegativeAverage where

    show = show . (fromNonNegativeAverage :: NonNegativeAverage -> Rational)


-- |
-- Safely construct a 'NonNegativeAverage' from a 'Word'.
{-# INLINE fromNonNegativeValue #-}
fromNonNegativeValue :: Word -> NonNegativeAverage
fromNonNegativeValue x = Avg x 1


-- |
-- Safely convert a 'NonNegativeAverage' to a 'Fractional' representation.
fromNonNegativeAverage :: Fractional r => NonNegativeAverage -> r
fromNonNegativeAverage (Avg n d) = num / den
  where
    num = fromIntegral n
    den = fromIntegral d
