-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Sankoff character analysis (cost and median)
--
-- This only works on static characters, and due to the traversal, only one
-- character will be received at a time.
--
-- Assumes binary trees.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Analysis.Parsimony.Dynamic.DirectOptimization
  ( OverlapFunction
  , directOptimizationPostorder
  , directOptimizationPostorderPairwise
  , directOptimizationPreorder
  , foreignPairwiseDO
--  , foreignThreeWayDO
  , naiveDO
  , naiveDOMemo
  , selectDynamicMetric
  , ukkonenDO
  , unboxedFullMatrixDO
  , unboxedSwappingDO
  , unboxedUkkonenFullSpaceDO
  , unboxedUkkonenSwappingDO
  ) where


import Analysis.Parsimony.Dynamic.DirectOptimization.Internal
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
