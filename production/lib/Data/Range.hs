-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Ranged
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.Range
  ( Range()
  , Ranged(..)
  -- * Constructors
  , fromTuple
  , lowerBound
  , upperBound
  -- * Set-like operations on Range
  , intersects
  , intersection
  , union
  -- * Specialized operations on Range for Additive/Continuous characters
  , closestState
  , largestClosed
  , smallestClosed
  ) where


newtype Range r = Range (r,r)

type family Bound (f :: * -> *)

class Ranged a where

    toRange :: a -> Range (Bound a)

    fromRange :: Range (Bound a) -> a -> a

    unitRange :: Bound a -> Range (Bound a)
    unitRange x = fromTuple (x,x)


fromTuple :: (r, r) -> Range r
fromTuple = Range


lowerBound :: Range r -> r
lowerBound (Range (lhs, _)) = lhs


upperBound :: Range r -> r
upperBound (Range (_, rhs)) = rhs


-- |
-- Finds the intersection of two intervals, the intersection being the smallest interval possible. Does
-- not assume there's an overlap.
intersection :: Range r -> Range r -> Range r
intersection lhs rhs = Range newLowerBound newUpperBound
    where
       newLowerBound = max (lowerBound lhs) (lowerBound rhs)
       newUpperBound = min (upperBound lhs) (upperBound rhs)


-- |
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest possible
-- value to the largest possible, considering the values in both intervals.
--
-- Works for overlapped or subsetted intervals, as well as non-overlapping intervals.
union :: Range r -> Range r -> Range r
union lhs rhs = Range newLowerBound newUpperBound
    where
        newLowerBound = min (lowerBound lhs) (lowerBound rhs)
        newUpperBound = max (upperBound lhs) (upperBound rhs)


-- |
-- The smallest closed interval is the smallext interval between two non-overlapping intervals, so the
-- largest value in the leftmost interval on the number line to the smallest value of the rightmost.
smallestClosed :: Range r -> Range r -> Range r
smallestClosed lhs rhs = Range newLowerBound newUpperBound
    where
        newLowerBound = min (upperBound lhs) (upperBound rhs)
        newUpperBound = max (lowerBound lhs) (lowerBound rhs)


-- |
-- The largest closed interval between the single value on the left and the interval on the right.
-- This is the equivalent of 'union', but works for a single value on the left, rather than an interval.
largestClosed :: r -> Range r -> Range r
largestClosed value interval = Range newLowerBound newUpperBound
    where
        newLowerBound = min value $ upperBound interval
        newUpperBound = max value $ lowerBound interval


-- |
-- The closest state is the closest value in the left interval to the right interval.
-- This assumes that there is no overlap between the intervals. If the two intervals intersect.
-- incorrect results will be returned.
closestState :: Range r -> Range r -> r
closestState lhs rhs
    | upperBound lhs < lowerBound rhs = upperBound lhs
    | otherwise                       = lowerBound lhs


-- |
-- True if there is any overlap between the two intervals.
intersects :: Range r -> Range r -> Bool
intersects lhs rhs = lowerBound rhs <= upperBound lhs
                  && upperBound rhs >= lowerBound lhs
