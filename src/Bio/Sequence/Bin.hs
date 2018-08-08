------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Bin
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Sequence.Bin
  ( StaticCharacterBin(..)
  , AdditiveBin()
  , ContinuousBin()
  , MetricBin()
  , NonAdditiveBin()
  , NonMetricBin()
  ) where


import           Bio.Sequence.Bin.Additive
import           Bio.Sequence.Bin.Class
import           Bio.Sequence.Bin.Continuous
import           Bio.Sequence.Bin.Metric
import           Bio.Sequence.Bin.NonAdditive
import           Bio.Sequence.Bin.NonMetric
