------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Bin.Metric
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Bio.Sequence.Bin.NonMetric
  ( NonMetricBin(..)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.Monoid          (mappend)
import Data.Semigroup
import Data.TCM


-- |
-- A bin of one or more non-metric characters and thier corresponding metadata.
--
-- Use '(<>)' to construct larger bins with differing metadata.
--
-- There is currently no singleton-like constructor!
data NonMetricBin s
   = NonMetricBin
   { characterDecoration :: s
   , tcmDefinition       :: TCM
   , metatdataBounds     :: SharedMetatdataIntervals
   } deriving (Eq, Functor, Show)


instance EncodedAmbiguityGroupContainer (NonMetricBin s) where

    {-# INLINE symbolCount #-}
    symbolCount = size . tcmDefinition
      

instance Semigroup s => Semigroup (NonMetricBin s) where

  lhs <> rhs =
    NonMetricBin
      { characterDecoration = characterDecoration lhs    <>     characterDecoration rhs
      , tcmDefinition       = tcmDefinition       lhs
      , metatdataBounds     = metatdataBounds     lhs `mappend` metatdataBounds     rhs
      }
