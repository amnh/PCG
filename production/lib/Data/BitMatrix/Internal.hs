-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitMatrix.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitMatrix.Internal where

import Data.Bifunctor
import Data.BitVector  hiding (foldl,foldr)
--import Data.Function.Memoize
import Data.List.Utility      (equalityOf)
import Data.Foldable
import Data.Maybe             (fromMaybe)
import Data.Monoid
import Data.MonoTraversable
import Test.QuickCheck hiding ((.&.))

-- | A data structure for storing a two dimensional array of bits.
--   Exposes row based monomorphic mapping & folding.
--
--   It is important to note the endianness of 'BitMatrix'.
--   The bit at position @(0,0)@ is displayed in the upper left hand corner when
--   the 'BitMatrix' is shown.
--   The bit at position @(i,x)@ will be of less significance than position @(i+1,x)@,
--   for the resulting xth 'BitVector' row when calling 'rows' on a 'BitMatrix'.
--   The bit at position @(x,i)@ will be of less significance than position @(x,i+1)@,
--   for the resulting xth 'BitVector' column when calling 'cols' on a 'BitMatrix'.
--
data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Eq)

-- | The row based element for monomorphic maps & folds.
type instance Element BitMatrix = BitVector

-- | A generating function for a 'BitMatrix'. Efficiently constructs a
--   'BitMatrix' of the specified dimensions with each bit defined by the result
--   of the supplied function.
--
--   /O(m + n)/
--
-- ==== __Examples__
--
-- >>> bitMatrix 3 2 $ const True
-- BitMatrix: 3 x 2
-- 111
-- 111
-- 111
--
-- >>> bitMatrix 4 7 $ \(i,j) -> i == j || i + j == 6
-- BitMatrix: 4 x 7
-- 1000001
-- 0100010
-- 0010100
-- 0001000
--
-- >>> bitMatrix 5 10 $ \(i,j) -> odd i /= even j
-- BitMatrix: 5 x 10
-- 1010101010
-- 0101010101
-- 1010101010
-- 0101010101
-- 1010101010
bitMatrix :: Int                 -- ^ Number of rows in the BitMatrix.
          -> Int                 -- ^ Number of columns in the BitMatrix.
          -> ((Int,Int) -> Bool) -- ^ Function to determine if a given index has a set bit.
          -> BitMatrix
bitMatrix m n f =
  case errorMsg of
    Just msg -> error msg
    Nothing  -> BitMatrix n . bitVec (m * n) . snd . foldl' g initialAccumulator $ [(i,j) | i <- [0..m-1], j <- [0..n-1]]
  where
    initialAccumulator :: (Integer, Integer)
    initialAccumulator = (1,0)
    g (!shiftRegister, !summation) i
      | f i       = (shiftRegister `shiftL` 1, shiftRegister + summation)
      | otherwise = (shiftRegister `shiftL` 1,                 summation)
    errorMsg
      | m <  0 && n <  0 = Just $ unwords [errorPrefix, errorRowCount, "also", errorColCount] <> "."
      | m <  0           = Just $ unwords [errorPrefix, errorRowCount] <> "."
      | n <  0           = Just $ unwords [errorPrefix, errorColCount] <> "."
      | m == 0 && n /= 0 = Just $ unwords [errorPrefix, errorZeroRows, errorZeroSuffix] <> "."
      | m /= 0 && n == 0 = Just $ unwords [errorPrefix, errorZeroCols, errorZeroSuffix] <> "."
      | otherwise        = Nothing
      where
        errorPrefix     = mconcat ["The call to bitMatrix ", show m, " ", show n, " f is malformed,"]
        errorRowCount   = mconcat ["the number of rows, "   , show m, ", is a negative number"]
        errorColCount   = mconcat ["the number of columns, ", show n, ", is a negative number"]
        errorZeroRows   = mconcat ["the number of rows was 0 but the number of columns, ", show n, ", was positive."]
        errorZeroCols   = mconcat ["the number of columns was 0 but the number of rows, ", show m, ", was positive."]
        errorZeroSuffix = "To construct the empty matrix, both rows and columns must be zero"

-- | Construct a 'BitMatrix' from a list of rows.
--   /O(m)/
fromRows :: Foldable t => t BitVector -> BitMatrix
fromRows xs
  | equalityOf width xs = result
  | otherwise           = error "fromRows: All the rows did not have the same width!"
  where
    -- concatenate the right way, dumb monoid instance of BV
    lhs `bvCat` rhs = bitVec (n + m) ((b `shiftL` n) + a)
      where
        n = width lhs
        m = width rhs
        a = nat   lhs
        b = nat   rhs
    result = case toList xs of
               []   -> BitMatrix 0 $ bitVec 0 (0 :: Integer)
               y:ys -> BitMatrix (width y) (if width y == 0 
                                            then bitVec (length xs) (0 :: Integer)
                                            else foldr1 bvCat $ y:ys)

-- | The number of columns in the 'BitMatrix'
--   /O(1)/
numCols :: BitMatrix -> Int
numCols (BitMatrix n _) = n

-- | The number of rows in the 'BitMatrix'
--   /O(1)/
numRows :: BitMatrix -> Int
numRows (BitMatrix n bv)
  | n == 0    = 0
  | otherwise = width bv `div` n

-- | The rows of the 'BitMatrix'
--   /O(m)/
rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix nCols bv)
    |  nRows == 0 || nCols == 0 = []
    |  nRows == 1               = [bv]
    |  otherwise                = (bv @@) <$> slices
    where
      nRows  = numRows bm
      slices = take nRows $ iterate ((nCols +) `bimap` (nCols +)) (nCols - 1, 0) --(start, end)

-- | Retreives a single row of the 'BitMatrix'.
--   Allows for unsafe indexing.
--   /O(1)/
row :: BitMatrix -> Int -> BitVector
row bm@(BitMatrix nCols bv) i
  | 0 <= i && i < nRows = bv @@ (left, right)
  | otherwise           = error errorMsg
  where
    -- It couldn't be more clear
    left     = nCols * (i + 1) - 1 -- BitVector is big-endian, so left is most-significant. 
    right    = left - nCols + 1
    nRows    = numRows bm
    errorMsg = unwords ["Index", show i, "is outside the range", rangeStr]
    rangeStr = mconcat ["[0..", show nRows, "]."]

-- | Determines if there are no set bits in the 'BitMatrix'
--   /O(1)/
isZeroMatrix :: BitMatrix -> Bool
isZeroMatrix (BitMatrix _ bv) = nat bv == 0

{-
col :: BitMatrix -> Int -> BitVector
col = undefined -- bit twiddle or math
-}

-- | Test if a bit is set at the given indices.
--   /O(1)/
isSet :: BitMatrix -> (Int, Int) -> Bool
(BitMatrix n bv) `isSet` (i,j) = bv `testBit` (n*i + j)

-- | Performs a row-wise monomporphic map over ther 'BitMatrix'.
instance MonoFunctor BitMatrix where
  omap f bm = BitMatrix (numCols bm) . mconcat . Prelude.reverse $ f <$> rows bm

-- | Performs a row-wise monomporphic fold over ther 'BitMatrix'.
instance MonoFoldable BitMatrix where
  -- | Map each element of a monomorphic container to a 'Monoid'
  -- and combine the results.
  {-# INLINE ofoldMap #-}
  ofoldMap f = ofoldr (mappend . f) mempty

  -- | Right-associative fold of a monomorphic container.
  {-# INLINE ofoldr #-}
  ofoldr f e = foldr f e . rows

  -- | Strict left-associative fold of a monomorphic container.
  {-# INLINE ofoldl' #-}
  ofoldl' f e = foldl' f e . rows

  -- | Right-associative fold of a monomorphic container with no base element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldr1Ex #-}
  ofoldr1Ex f = foldr1 f . rows

  -- | Strict left-associative fold of a monomorphic container with no base
  -- element.
  --
  -- Note: this is a partial function. On an empty 'MonoFoldable', it will
  -- throw an exception.
  --
  -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
  {-# INLINE ofoldl1Ex' #-}
  ofoldl1Ex' f = foldl1 f . rows

  {-# INLINE onull #-}
  onull (BitMatrix 0 _) = True
  onull  _              = False

  {-# INLINE olength #-}
  olength = numRows

-- | Performs a row-wise monomporphic traversal over ther 'BitMatrix'.
instance MonoTraversable BitMatrix where
    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    {-# INLINE otraverse #-}
    otraverse f bm = fmap (BitMatrix (numCols bm) . mconcat) . traverse f $ rows bm

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    {-# INLINE omapM #-}
    omapM = otraverse
{-

-- | (✔)
instance Memoizable BitMatrix where
    memoize f (BitMatrix n bv) = memoize (f . BitMatrix n) bv

-- | (✔)
instance Memoizable BV where
    memoize f char = memoize (f . bitVec w) (nat char)
      where
        w = width char
-}

-- | For binary operations we (perhaps erroneously) assume equal column and row
--   dimensions
instance Bits BitMatrix where
    (.&.)        (BitMatrix c lhs) (BitMatrix _ rhs) = BitMatrix c $ lhs  .&.  rhs
    (.|.)        (BitMatrix c lhs) (BitMatrix _ rhs) = BitMatrix c $ lhs  .|.  rhs
    xor          (BitMatrix c lhs) (BitMatrix _ rhs) = BitMatrix c $ lhs `xor` rhs
    complement   (BitMatrix c b)                     = BitMatrix c $ complement b
    shift        (BitMatrix c b) n                   = BitMatrix c $ b `shift`  n
    rotate       (BitMatrix c b) n                   = BitMatrix c $ b `rotate` n
    setBit       (BitMatrix c b) i                   = BitMatrix c $ b `setBit` i
    testBit      (BitMatrix _ b) i                   = b `testBit` i -- (width b - i + 1)
    bit i                                            = BitMatrix 1 $ bit i
    bitSize                                          = fromMaybe 0 . bitSizeMaybe
    bitSizeMaybe (BitMatrix _ b)                     = bitSizeMaybe b
    isSigned     (BitMatrix _ b)                     = isSigned b
    popCount     (BitMatrix _ b)                     = popCount b

-- | Resulting matricies will have at /least/ one row and one column.
instance Arbitrary BitMatrix where
    arbitrary = do 
        colCount <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 20) 
        rowCount <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 20)
        let rVal = choose (0, 2 ^ colCount -1) :: Gen Integer
        bitRows  <- vectorOf rowCount rVal
        pure . fromRows $ bitVec colCount <$> bitRows

-- | (✔)
instance Show BitMatrix where
    show bm = headerLine <> matrixLines
      where
        renderRow   = foldl (\acc e -> (if e then '1' else '0') : acc) "" . toBits
        matrixLines = unlines $ renderRow <$> rows bm          
        headerLine  = '\n' : unwords
                    [ "BitMatrix:"
                    , show $ numRows bm
                    , "x"
                    , show $ numCols bm
                    , "\n"
                    ]
