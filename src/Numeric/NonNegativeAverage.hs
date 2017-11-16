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

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Numeric.NonNegativeAverage
  ( NonNegativeAverage()
  , fromNonNegativeAverage
  , fromNonNegativeValue
  ) where

import Control.DeepSeq
import Data.Data
import Data.Ratio
import Data.Semigroup
import Foreign.Storable
import GHC.Generics


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
newtype NonNegativeAverage = Avg (Ratio Word)
  deriving (Data, Eq, Generic, Ord, Storable)


instance Bounded NonNegativeAverage where

    maxBound = Avg $ maxBound % 1

    minBound = Avg $ 0 % 1


instance NFData NonNegativeAverage


instance Semigroup NonNegativeAverage where

    (<>) (Avg lhs) (Avg rhs) = Avg $ num % den
      where
        num = numerator   lhs + numerator   rhs
        den = denominator lhs + denominator rhs


instance Show NonNegativeAverage where

    show = show . (fromNonNegativeAverage :: NonNegativeAverage -> Double)


{-# INLINE fromNonNegativeValue #-}
fromNonNegativeValue :: Word -> NonNegativeAverage
fromNonNegativeValue = Avg . (% 1)


fromNonNegativeAverage :: Fractional r => NonNegativeAverage -> r
fromNonNegativeAverage (Avg avg) = num / den
  where
    num = fromIntegral (numerator   avg)
    den = fromIntegral (denominator avg)
