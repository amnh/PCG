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
  ( Bound()
  , Range()
  , Ranged(..)
  -- * Constructors
  , fromTuple
  , fromTupleWithPrecision
  -- * Accessors
  , lowerBound
  , upperBound
  , precision
  -- * Set-like operations on Range
  , intersects
  , intersection
  , union
  -- * Specialized operations on Range for Additive/Continuous characters
  , closestStateTo
  , largestClosed
  , smallestClosed
  , threeWayRange
  ) where


newtype Range r = Range (r, r, Maybe Int)
  deriving (Eq)


type family Bound (f :: *)


class Num (Bound a) => Ranged a where

    toRange :: a -> Range (Bound a)

    fromRange :: Range (Bound a) -> a

    zeroRange :: a -> Range (Bound a)


instance Show r => Show (Range r) where

    show (Range (x,y,_)) = mconcat [ "[" , show x, ", ", show y, "]" ]
    

fromTuple :: (r, r) -> Range r
fromTuple (x,y) = Range (x, y, Nothing)


fromTupleWithPrecision :: (r, r) -> Int -> Range r
fromTupleWithPrecision (x,y) d = Range (x, y, Just d)


-- |
-- Represents the precision of the Range.
-- A Nothing value represents infinite precision.
-- A Just    value represents a finite precision.
precision :: Range r -> Maybe Int
precision  (Range (_, _, p)) = p


lowerBound :: Range r -> r
lowerBound (Range (lhs, _, _)) = lhs


upperBound :: Range r -> r
upperBound (Range (_, rhs, _)) = rhs


-- |
-- /O(1)/
--
-- True if there is any overlap between the two intervals.
intersects :: Ord r => Range r -> Range r -> Bool
intersects lhs rhs = lowerBound rhs <= upperBound lhs
                  && upperBound rhs >= lowerBound lhs


-- |
-- /O(1)/
--
-- Finds the intersection of two intervals, the intersection being the smallest interval possible. Does
-- not assume there's an overlap.
intersection :: Ord r => Range r -> Range r -> Range r
intersection lhs rhs = Range (newLowerBound, newUpperBound, precision lhs)
    where
       newLowerBound = max (lowerBound lhs) (lowerBound rhs)
       newUpperBound = min (upperBound lhs) (upperBound rhs)


-- |
-- /O(1)/
--
-- Finds the union of two intervals, where the union is the largest interval possible, i.e. from the smallest possible
-- value to the largest possible, considering the values in both intervals.
--
-- Works for overlapped or subsetted intervals, as well as non-overlapping intervals.
union :: Ord r => Range r -> Range r -> Range r
union lhs rhs = Range (newLowerBound, newUpperBound, precision lhs)
    where
        newLowerBound = min (lowerBound lhs) (lowerBound rhs)
        newUpperBound = max (upperBound lhs) (upperBound rhs)


-- |
-- /O(1)/
--
-- The closest state is the closest value in the left interval to the right interval.
-- This assumes that there is no overlap between the intervals. If the two intervals intersect.
-- incorrect results will be returned.
closestStateTo :: Ord r => Range r -> Range r -> r
closestStateTo lhs rhs
    | upperBound lhs < lowerBound rhs = upperBound lhs
    | otherwise                       = lowerBound lhs


-- |
-- /O(1)/
--
-- The smallest closed interval is the smallext interval between two non-overlapping intervals, so the
-- largest value in the leftmost interval on the number line to the smallest value of the rightmost.
smallestClosed :: Ord r => Range r -> Range r -> Range r
smallestClosed lhs rhs = Range (newLowerBound, newUpperBound, precision lhs)
    where
        newLowerBound = min (upperBound lhs) (upperBound rhs)
        newUpperBound = max (lowerBound lhs) (lowerBound rhs)


-- |
-- /O(1)/
--
-- The largest closed interval between the single value on the left and the interval on the right.
-- This is the equivalent of 'union', but works for a single value on the left, rather than an interval.
largestClosed :: Ord r => Range r -> r -> Range r
largestClosed interval value = Range (newLowerBound, newUpperBound, precision interval)
    where
        newLowerBound = min value $ upperBound interval
        newUpperBound = max value $ lowerBound interval


threeWayRange :: Ord r => Range r -> Range r -> Range r -> Range r
threeWayRange ancestoralInterval selfInterval descendantInterval = Range (newLowerBound, newUpperBound, precision selfInterval) 
    where
        selfClosestBound  =       selfInterval `closestStateTo` ancestoralInterval 
        heirClosestBound  = descendantInterval `closestStateTo` ancestoralInterval 
        newLowerBound     = min selfClosestBound heirClosestBound
        newUpperBound     = max selfClosestBound heirClosestBound
