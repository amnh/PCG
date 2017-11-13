-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.PositiveAverage
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Numeric.PositiveAverage
  ( PositiveAverage()
  , fromPositiveAverage
  ) where

import Control.DeepSeq
import Data.Data
import Data.Ratio
import Data.Semigroup
import Foreign.Storable
import GHC.Generics


-- |
-- A type defines an average of positive numbers.
--
-- This is a newtyped 'Ratio Word' for efficiency purposes.
--
-- Use the `Enum` instance's 'toEnum' method to create a single average value.
--
-- Use the `Semigroup` operator '(<>)' to combine two 'PositiveAverage' values.
--
-- All instance operations are /O(1)/.
newtype PositiveAverage = Avg (Ratio Word)
  deriving (Data, Enum, Eq, Generic, Ord, Storable)


instance Bounded PositiveAverage where

    maxBound = Avg $ maxBound % 1

    minBound = Avg $ 1 % maxBound


instance NFData PositiveAverage


instance Semigroup PositiveAverage where

    (<>) (Avg lhs) (Avg rhs) = Avg $ num % den
      where
        num = numerator   lhs + numerator   rhs
        den = denominator lhs + denominator rhs


instance Show PositiveAverage where

    show = show . (fromPositiveAverage :: PositiveAverage -> Double)


fromPositiveAverage :: Fractional r => PositiveAverage -> r
fromPositiveAverage (Avg avg) = num / den
  where
    num = fromIntegral (numerator   avg)
    den = fromIntegral (denominator avg)
