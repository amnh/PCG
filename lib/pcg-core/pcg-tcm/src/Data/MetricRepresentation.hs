-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.MetricRepresentation
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveFoldable   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnboxedSums      #-}

module Data.MetricRepresentation
  ( MetricRepresentation(..)
  , retreiveSCM
  , retreivePairwiseTCM
  , retreiveThreewayTCM
  ) where

import Data.TCM        as TCM
import Control.DeepSeq
import Data.Bits
import Data.Foldable
import Data.Ord        (comparing)
import Data.Range
import GHC.Generics    hiding (to)


-- |
-- Represents the metric for some discrete characters or dynamic characters.
-- The representation notes if the discrete metric or the L1 norm are the
-- specified metric for the character. If either of these metrics are specifed,
-- specialized functions which are more efficient will be returned when
-- retreiving the pairwise of threeway transition cost matrix.
--
-- It is important to use this type in the metadata decorations rather than store
-- a function because a function cannot be contained in a compact region.
--
-- Use the elimination functions 'retreiveSCM', 'retreivePairwiseTCM', and 'retreiveThreewayTCM'
-- to the retreive the desired functions.
data  MetricRepresentation a
    = ExplicitLayout {-# UNPACK #-} !TCM !a
    | DiscreteMetric
    | LinearNorm
    deriving (Eq, Foldable, Functor, Generic, NFData)


retreiveSCM :: MetricRepresentation a -> Word -> Word -> Word
retreiveSCM (ExplicitLayout tcm _) = \i j -> toEnum . fromEnum $ tcm TCM.! (i,j)
retreiveSCM DiscreteMetric         = \i j -> if i == j then 0 else 1
retreiveSCM LinearNorm             = \i j -> max i j - min i j


retreivePairwiseTCM
  :: ( Bits c
     , Bound c ~ Word
     , Ranged c
     )
  => (TCM -> a -> c -> c -> (c, Word))
  -> MetricRepresentation a
  -> c
  -> c
  -> (c, Word)
retreivePairwiseTCM f (ExplicitLayout a b) = f a b
retreivePairwiseTCM _ DiscreteMetric       =  discreteMetricPairwiseLogic
retreivePairwiseTCM _ LinearNorm           = firstLinearNormPairwiseLogic


retreiveThreewayTCM
  :: ( Bits c
     , Bound c ~ Word
     , Ranged c
     )
  => (TCM -> a -> c -> c -> c -> (c, Word))
  -> MetricRepresentation a
  -> c
  -> c
  -> c
  -> (c, Word)
retreiveThreewayTCM f (ExplicitLayout a b) = f a b
retreiveThreewayTCM _ DiscreteMetric       =  discreteMetricThreewayLogic
retreiveThreewayTCM _ LinearNorm           = firstLinearNormThreewayLogic


discreteMetricPairwiseLogic
  :: ( Bits a
     , Num b
     )
  => a
  -> a
  -> (a, b)
discreteMetricPairwiseLogic lhs rhs
  | popCount intersect > 0 = (intersect, 0)
  | otherwise              = (  unioned, 1)
  where
    unioned   = lhs .|. rhs
    intersect = lhs .&. rhs


-- |
-- if           x    ⋂    y    ⋂    z    ≠ Ø ⮕  (    x    ⋂    y    ⋂    z    , 0)
--
-- else if   (x ⋂ y) ⋃ (x ⋂ z) ⋃ (y ⋂ z) ≠ Ø ⮕  ( (x ⋂ y) ⋃ (x ⋂ z) ⋃ (y ⋂ z) , 1)
--
-- otherwise                                 ⮕  (    x    ⋃    y    ⋃    z    , 2)
--
--
discreteMetricThreewayLogic
  :: ( Bits a
     , Num b
     )
  => a
  -> a
  -> a
  -> (a, b)
discreteMetricThreewayLogic x y z
  | popCount fullIntersection > 0 = (fullIntersection, 0)
  | popCount joinIntersection > 0 = (joinIntersection, 1)
  | otherwise                     = (fullUnion,        2)
  where
    fullUnion        = x .|. y .|. z
    fullIntersection = x .&. y .&. z
    joinIntersection = (x .&. y) .|. (x .&. z) .|. (y .&. z)


firstLinearNormPairwiseLogic
  :: ( Ord (Bound a)
     , Ranged a
     , Ranged b
     , Ranged c
     , Bound b ~ Bound a
     , Bound c ~ Bound a
     )
  => a
  -> b
  -> (c, Bound a)
firstLinearNormPairwiseLogic lhs rhs = (fromRange newInterval, cost)
  where
    lhs' = toRange lhs
    rhs' = toRange rhs

    newInterval
      | isOverlapping = lhs' `intersection`   rhs'
      | otherwise     = lhs' `smallestClosed` rhs'
    isOverlapping     = lhs' `intersects`     rhs'

    cost
      | isOverlapping = 0
      | otherwise     = upperBound newInterval - lowerBound newInterval


firstLinearNormThreewayLogic
  :: ( Ord (Bound a)
     , Ranged a
     , Ranged b
     , Ranged c
     , Ranged d
     , Bound b ~ Bound a
     , Bound c ~ Bound a
     , Bound d ~ Bound a
     )
  => a
  -> b
  -> c
  -> (d, Bound a)
firstLinearNormThreewayLogic x y z
  | and intersections = (fromRange fullIntersection, 0)
  | or  intersections = paritalIntersection
  | otherwise         = (fromRange y', lowerBound y' - upperBound x' + lowerBound z' - upperBound y')
  where
    (x', y', z') =
      let rx = toRange x
          ry = toRange y
          rz = toRange z
      in if lowerBound rx <= lowerBound ry
         then if lowerBound rx <= lowerBound rz
              then if lowerBound ry <= lowerBound rz
                   then (rx, ry, rz)
                   else (rx, rz, ry)
              else (rz, rx, ry)
         else if lowerBound ry <= lowerBound rz
              then if lowerBound rx <= lowerBound rz
                   then (ry, rx, rz)
                   else (ry, rz, rx)
              else (rz, ry, rx)

    intersections =
        [ x' `intersects` y'
        , x' `intersects` z'
        , y' `intersects` z'
        ]

    fullIntersection = x' `intersection` y' `intersection` z'

    paritalIntersection = minimumBy (comparing snd)
        [ getSmallestClosed x' y' z'
        , getSmallestClosed x' z' y'
        , getSmallestClosed y' z' x'
        ]

    getSmallestClosed i j k =
      let v = (i `intersection` j) `smallestClosed` k
      in  (fromRange v, upperBound v - lowerBound v)
