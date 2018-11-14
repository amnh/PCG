-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.MetricRepresentation
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

module Bio.Metadata.MetricRepresentation
  ( MetricRepresentation(..)
  , retreiveSCM
  , retreivePairwiseTCM
  , retreiveThreewayTCM
  ) where

--import Bio.Character.Exportable
--import Bio.Metadata.CharacterName
--import Bio.Metadata.Discrete
--import Bio.Metadata.DiscreteWithTCM.Class
import Control.DeepSeq
--import Control.Lens
--import Data.Alphabet
import Data.Bits
--import Data.List                          (intercalate)
import Data.Range
import Data.TCM        as TCM
--import Data.TCM.Memoized
import GHC.Generics    hiding (to)
--import Text.XML


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


-- |
-- if           x    ⋂    y    ⋂    z    ≠ Ø ⮕  (    x    ⋂    y    ⋂    z    , 0)
--
-- else if   (x ⋂ y) ⋃ (x ⋂ z) ⋃ (y ⋂ z) ≠ Ø ⮕  ( (x ⋂ y) ⋃ (x ⋂ z) ⋃ (y ⋂ z) , 1)
--
-- otherwise                                 ⮕  (    x    ⋃    y    ⋃    z    , 2)
--
--
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
firstLinearNormThreewayLogic x y z = (fromRange newInterval, cost)
  where
    x' = toRange x
    y' = toRange y

    newInterval
      | isOverlapping = x' `intersection`   y'
      | otherwise     = x' `smallestClosed` y'
    isOverlapping     = x' `intersects`     y'

    cost
      | isOverlapping = 0
      | otherwise     = upperBound newInterval - lowerBound newInterval
