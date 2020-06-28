-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Pairwise direct optimization alignment functions using a variety of techniques.
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
  ( OverlapFunction
  , filterGaps
  , foreignPairwiseDO
--  , foreignThreeWayDO
  , naiveDO
  , naiveDOMemo
  , ukkonenDO
  , unboxedFullMatrixDO
  , unboxedSwappingDO
  , unboxedUkkonenFullSpaceDO
  , unboxedUkkonenSwappingDO
  ) where


import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedFullMatrix
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenFullSpace
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenSwapping
