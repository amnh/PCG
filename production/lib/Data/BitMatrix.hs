{-# LANGUAGE TypeFamilies #-}
module Data.BitMatrix
  ( BitMatrix()
  , bitMatrix
  , fromRows
  , numCols
  , numRows
  , rows
  , row
  ) where

import Data.Bifunctor
import Data.BitVector hiding (foldr)
import Data.Foldable
import Data.Monoid
import Data.MonoTraversable

data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Show)

type instance Element BitMatrix = BitVector

bitMatrix :: Int -> Int -> ((Int,Int) -> Bool) -> BitMatrix
bitMatrix m n f =
  case errorMsg m n of
    Just msg -> error msg
    Nothing  -> BitMatrix n . bitVec (m*n) . snd . foldl' g initialAccumulator $ [(i,j) | i <- [0..m-1], j <- [0..n-1]]
  where
    initialAccumulator :: (Integer, Integer)
    initialAccumulator = (1,0)
    g (exponent, summation) index
      | f index   = (exponent `shiftL` 1, exponent + summation)
      | otherwise = (exponent `shiftL` 1,            summation)
    errorMsg m n
      | m <  0 && n <  0 = Just $ unwords [errorPrefix, errorRowCount, "also", errorColCount] <> "."
      | m <  0           = Just $ unwords [errorPrefix, errorRowCount] <> "."
      | n <  0           = Just $ unwords [errorPrefix, errorColCount] <> "."
      | m == 0 && n != 0 = Just $ unwords [errorPrefix, errorzeroRows, errorZeroSuffix] <> "."
      | m != 0 && n == 0 = Just $ unwords [errorPrefix, errorzeroCols, errorZeroSuffix] <> "."
      | otherwise        = Nothing
      where
        errorPrefix     = mconcat ["The call to bitMatrix ", show m, " ", show n, "f is malformed,"]
        errorRowCount   = mconcat ["the number of rows "   , show m, "is a negative number"]
        errorColCount   = mconcat ["the number of columns ", show n, "is a negative number"]
        errorZeroRows   = mconcat ["the number of rows was 0 but the number of columns ", show n, " was positive."]
        errorZeroCols   = mconcat ["the number of columns was 0 but the number of rows ", show m, " was positive."]
        errorZeroSuffix = "To construct the empty matrix, both rows and columns must be zero"

fromRows :: Foldable t => t BitVector -> BitMatrix
fromRows xs
  | null xs   = error "The call to fromRows was given an empty Foldabble structure."
  | otherwise = BitMatrix n $ mconcat xs'
  where
    xs' = toList xs
    n   = width $ head xs'

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
row (BitMatrix n bv) i = bv @@ ((n+1) * i - 1, n * i)

{-
col :: BitMatrix -> Int -> BitVector
col = undefined -- bit twiddle or math
-}

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
