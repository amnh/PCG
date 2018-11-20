-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for memoising a computation of a vector.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Utility
  ( DVector(..)
  , generateMemo
  , zip3
  ) where

import Data.Tuple.Utility
import Data.Vector        as V hiding (zip3)
import Prelude            hiding (zip3)


-- |
-- A "difference" vector which describe how to perform a memoize, full
-- transformation over a vector. Useful when you want to compose multiple
-- open-recursion memoizations for a vector.
--
-- Use 'generateMemo' to produce the concrete vector from the "difference"
-- vector specification.
newtype DVector a = DVector {getDVector :: (Int -> a) -> Int -> a}


-- |
-- This will generate a function in a memoized fashion across the range of the vector.
-- values of the function outside the range.
generateMemo :: forall a
  .  Int        -- ^ Range of memoization
  -> DVector a  -- ^ Unmemoized function with open recursion
  -> Vector a   -- ^ Memoized vector
generateMemo range dVector = memoizedVect
  where
    openRecurseFn = getDVector dVector

    memoizedFunction :: Int -> a
    memoizedFunction i = memoizedVect ! i

    memoizedVect :: V.Vector a
    memoizedVect = generate range (openRecurseFn memoizedFunction)


-- |
-- Helper function to zip together three openly recursive vectors.
zip3 :: DVector a -> DVector b -> DVector c -> DVector (a, b, c)
zip3 dVectorA dVectorB dVectorC =
  let
    genFnA = getDVector dVectorA
    genFnB = getDVector dVectorB
    genFnC = getDVector dVectorC
  in
    DVector $ \recurseFn ind ->
      let
        recurseFnA = proj3_1 . recurseFn
        recurseFnB = proj3_2 . recurseFn
        recurseFnC = proj3_3 . recurseFn
        aVal       = genFnA recurseFnA ind
        bVal       = genFnB recurseFnB ind
        cVal       = genFnC recurseFnC ind
      in
        (aVal, bVal, cVal)
