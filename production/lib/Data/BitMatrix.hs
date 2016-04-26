{-# LANGUAGE TypeFamilies #-}
module Data.BitMatrix where

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

numCols :: BitMatrix -> Int
numCols (BitMatrix n _) = n

numRows :: BitMatrix -> Int
numRows (BitMatrix n bv) = width bv `div` n

rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix n bv) = (bv @@) <$> slices
  where
    m = numRows bm
    slices = take m $ iterate ((+n) `bimap` (+n)) (n-1, 0)
        
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
