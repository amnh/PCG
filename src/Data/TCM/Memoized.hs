-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM.Memoized
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.TCM.Memoized
  ( FFI.MemoizedCostMatrix
  , generateMemoizedTransitionCostMatrix
  , FFI.getMedianAndCost2D
  , FFI.getMedianAndCost3D
  ) where

import qualified Data.TCM.Memoized.FFI as FFI


-- |
-- /O(n^2)/ where @n@ is the alphabet size.
--
-- Generate a memoized TCM by supplying the size of the symbol alphabet and the
-- generating function for unambiguous symbol change transistions.
-- When this function is invoked it *strictly* constructs transition matrix for a set of *unambiguous*
-- singleton symbols, where the cells of the matrix hold the costs and medians of transitions
-- between unambiguous symbols.
-- A memoized TCM calculates the cost and medians of *ambiguous* symbol sets in a
-- lazy, memoized manner.
--
-- *Note:* The collection of ambiguous symbol set transitions is the powerset of
-- the collection of unambiguous singleton symbols. Laziness and memoization is
-- requisite for efficient computation on any non-trivial alphabet size.
generateMemoizedTransitionCostMatrix
  :: Word                   -- ^ Alphabet size
  -> (Word -> Word -> Word) -- ^ Generating function
  -> FFI.MemoizedCostMatrix
generateMemoizedTransitionCostMatrix = FFI.getMemoizedCostMatrix

{-
-- Causes ambiguity with Data.TCM.(!)
(!) :: Exportable s => FFI.MemoizedCostMatrix -> (s, s) -> (s, Word)
(!) memo (x,y) = FFI.getMedianAndCost memo x y
-}
