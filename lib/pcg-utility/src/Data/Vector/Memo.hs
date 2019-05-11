-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MemoVector
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

{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Vector.Memo
  ( DVector(..)
  , generateMemo
  , liftExp2
  , zip2
  , pair3
  , pair4
  , zip3
  , zip4
  ) where

import Control.Arrow ((&&&))
import Data.Coerce
import Data.Vector   as V hiding (zip3, zip4)
import Prelude       hiding (zip3)


type Endo a = (a -> a)


-- |
-- A "difference" vector which describe how to perform a memoize, full
-- transformation over a vector. Useful when you want to compose multiple
-- open-recursion memoizations for a vector.
--
-- Use 'generateMemo' to produce the concrete vector from the "difference"
-- vector specification.
newtype DVector a = DVector { getDVector :: Endo (Int -> a)}


-- |
-- This will generate a function in a memoized fashion across the range of the vector.
-- values of the function outside the range.
generateMemo :: forall a
  .  Int        -- ^ Range of memoization
  -> DVector a  -- ^ Unmemoized function with open recursion
  -> Vector a   -- ^ Memoized vector
generateMemo range dVector = memoizedVect
  where
    openRecurseFn = coerce dVector

    memoizedFunction :: Int -> a
    memoizedFunction i = memoizedVect ! i

    memoizedVect :: V.Vector a
    memoizedVect = generate range (openRecurseFn memoizedFunction)


-- |
-- This is NOT the catresiona product...
class Cartesian f where

    pair :: f a -> f b -> f (a, b)


class ExpFunctor f where

    xmap :: (a -> b) -> (b -> a) -> f a -> f b


instance ExpFunctor DVector where

    xmap :: forall a b . (a -> b) -> (b -> a) -> DVector a -> DVector b
    xmap = coerce $ xmapE @Int @a @b


instance Cartesian DVector where

    pair = zip2


-- |
-- Lifts a binary function into the 'Expfunctor'.
{-# INLINE liftExp2 #-}
liftExp2 :: (ExpFunctor f, Cartesian f)
  => (a -> b -> c)
  -> (c -> (a, b))
  -> f a -> f b -> f c
liftExp2 op split fa fb =
  let fab = pair fa fb
      op' = uncurry op
  in  xmap op' split fab


{-# INLINE zipE #-}
zipE :: forall e x y . Endo (e -> x) -> Endo (e -> y) -> Endo (e -> (x, y))
zipE fa fb fab = fa (fst . fab) &&& fb (snd . fab)


{-# INLINE xmapE #-}
xmapE :: forall e x y . (x -> y) -> (y -> x) -> Endo (e -> x) -> Endo (e -> y)
xmapE to from fca fcb = to . fca (from . fcb)

-- |
-- Zips together two openly recursive vectors.
{-# INLINE zip2 #-}
zip2 :: forall a b. DVector a -> DVector b -> DVector (a, b)
zip2  = coerce $ zipE @Int @a @b


-- |
-- Zips together three openly recursive vectors.
{-# INLINE pair3 #-}
pair3 :: (ExpFunctor f, Cartesian f) => f a -> f b -> f c -> f (a, b, c)
pair3 fa fb fc = xmap to from $ pair (pair fa fb) fc
  where
    to   ((a,b),c) = (a,b,c)
    from (a,b,c)   = ((a,b),c)

-- |
-- Zips together four openly recursive vectors.
{-# INLINE pair4 #-}
pair4 :: (ExpFunctor f, Cartesian f) =>  f a -> f b -> f c -> f d -> f (a,b,c,d)
pair4 fa fb fc fd = xmap to from $ pair (pair3 fa fb fc) fd
  where
    to   ((a,b,c), d) = (a,b,c,d)
    from (a,b,c,d)    = ((a,b,c),d)


-- |
-- Helper function to zip together three openly recursive vectors.
zip3 :: DVector a -> DVector b -> DVector c -> DVector (a, b, c)
zip3 = pair3


-- |
-- Helper function to zip together three openly recursive vectors.
zip4 :: DVector a -> DVector b -> DVector c -> DVector d ->  DVector (a, b, c, d)
zip4 = pair4
