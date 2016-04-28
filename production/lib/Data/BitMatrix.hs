{-# LANGUAGE TypeFamilies #-}
module Data.BitMatrix
  ( BitMatrix()
  , numCols
  , numRows
  , rows
  , row
  ) where

import Data.Bifunctor
import Data.Bits
import Data.BitVector hiding (foldr)
import Data.Foldable
import Data.Monoid
import Data.MonoTraversable

data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Show)

type instance Element BitMatrix = BitVector

bitMatrix :: Int -> Int -> ((Int,Int) -> Bool) -> BitMatrix
bitMatrix = undefined

numCols :: BitMatrix -> Int
numCols (BitMatrix n _) = n

numRows :: BitMatrix -> Int
numRows (BitMatrix n bv) = width bv `div` n

rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix n bv) = (bv @@) <$> slices
  where
    m = numRows bm
    slices = take m $ iterate ((+n) `bimap` (+n)) (n-1, 0)

row :: BitMatrix -> Int -> BitVector
row bm@(BitMatrix n bv) i = bv @@ ((n+1) * i - 1, n * i)

col :: BitMatrix -> Int -> BitVector
col = undefined -- bit twiddle or math

instance MonoFunctor BitMatrix where
  omap f bm = BitMatrix (numCols bm) . mconcat $ f <$> rows bm

instance MonoFoldable BitMatrix where
  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINE ofoldMap #-}

  -- | Right-associative fold of a monomorphic container.
  ofoldr f e = foldr f e . rows
  {-# INLINE ofoldr #-}

  -- | Strict left-associative fold of a monomorphic container.
  ofoldl' f e = foldl' f e . rows
  {-# INLINE ofoldl' #-}

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  ofoldr1Ex f = foldr1 f . rows
  {-# INLINE ofoldr1Ex #-}

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  ofoldl1Ex' f = foldl1 f . rows
  {-# INLINE ofoldl1Ex' #-}

-- | Monomorphic containers that can be traversed from left to right.
instance MonoTraversable BitMatrix where
  -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    otraverse f bm = fmap (BitMatrix (numCols bm) . mconcat) . traverse f $ rows bm
    {-# INLINE otraverse #-}

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    omapM = otraverse
    {-# INLINE omapM #-}
