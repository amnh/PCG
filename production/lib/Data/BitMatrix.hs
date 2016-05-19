-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitMatrix
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A matrix of bits with some useful operations.
-- Exposes row-based monomorphic maps, folds, and traversals.
-- Intended to be used by multiple datatypes for space efficient character
-- state encoding and packing.
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.BitMatrix
  ( BitMatrix()
  , bitMatrix
  , fromRows
  , isZeroMatrix
  , numCols
  , numRows
  , rows
  , row
  ) where

import Data.Bifunctor
import Data.BitVector  hiding (foldl,foldr)
import Data.List.Utility      (equalityOf)
import Data.Function.Memoize
import Data.Foldable
import Data.Maybe             (fromMaybe)
import Data.Monoid
import Data.MonoTraversable
import Test.QuickCheck.Arbitrary.Instances() -- Nescissary for Arbitrary instances
import Test.QuickCheck hiding ((.&.))

-- | A data structure for storing a two dimensional array of bits.
--   Exposes row based monomorphic mapping & folding.
data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Eq)

-- | The row based element for monomorphic maps & folds.
type instance Element BitMatrix = BitVector

-- | A generating function for a 'BitMatrix'. Efficiently constructs a
--   'BitMatrix' of the specified dimensions with each bit defined by the result
--   of the supplied function.
bitMatrix :: Int                 -- ^ Number of rows in the BitMatrix.
          -> Int                 -- ^ Number of columns in the BitMatrix.
          -> ((Int,Int) -> Bool) -- ^ Function to determine if a given index has a set bit.
          -> BitMatrix
bitMatrix m n f =
  case errorMsg of
    Just msg -> error msg
    Nothing  -> BitMatrix n . bitVec (m*n) . snd . foldl' g initialAccumulator $ [(i,j) | i <- [0..m-1], j <- [0..n-1]]
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
fromRows :: Foldable t => t BitVector -> BitMatrix
fromRows xs
  | equalityOf width xs = result
  | otherwise           = error $ "fromRows: All the rows did not have the same width!"
  where
    result = case toList xs of
               []   -> BitMatrix 0 $ bitVec 0 (0 :: Integer)
               y:ys -> BitMatrix (width y) (if width y == 0 
                                            then bitVec (length xs) (0 :: Integer)
                                            else mconcat $ y:ys)

-- | The number of columns in the 'BitMatrix'
numCols :: BitMatrix -> Int
numCols (BitMatrix n _) = n

-- | The number of rows in the 'BitMatrix'
numRows :: BitMatrix -> Int
numRows (BitMatrix n bv)
  | n == 0    = 0
  | otherwise = width bv `div` n

-- | The rows of the 'BitMatrix'
rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix nCols bv) 
    | nRows <= 1 = [bv]
    | nCols == 0 = take nRows $ repeat $ bitVec 0 (0 :: Integer)
    | otherwise  = (bv @@) <$> slices
          where
            nRows = numRows bm
            slices = take nRows $ iterate ((nCols `subtract`) `bimap` (nCols `subtract`)) (start, end)
            start  = nCols * nRows - 1 -- start at most-significant (left end of bv)
            end    = (nRows - 1) * nCols -- should eventually become 0


-- | Retreives a single row of the 'BitMatrix'.
--   Allows for unsafe indexing.
row :: BitMatrix -> Int -> BitVector
row bm@(BitMatrix nCols bv) i
  | 0 <= i && i < nRows = bv @@ (left, right)
  | otherwise       = error errorMsg
  where
    -- It couldn't be more clear
    left     = nCols * (i + 1) - 1 -- BitVector is big-endian, so left is most-significant. 
    right    = left - nCols + 1
    nRows    = numRows bm
    errorMsg = unwords ["Index", show i, "is outside the range", rangeStr]
    rangeStr = mconcat ["[0..", show nRows, "]."]

isZeroMatrix :: BitMatrix -> Bool
isZeroMatrix (BitMatrix _ bv) = nat bv == 0

{-
col :: BitMatrix -> Int -> BitVector
col = undefined -- bit twiddle or math
-}

-- | Performs a row-wise monomporphic map over ther 'BitMatrix'.
instance MonoFunctor BitMatrix where
  omap f bm = BitMatrix (numCols bm) . mconcat $ f <$> rows bm

-- | Performs a row-wise monomporphic fold over ther 'BitMatrix'.
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

  onull (BitMatrix 0 _) = True
  onull  _              = False
  {-# INLINE onull #-}

-- | Performs a row-wise monomporphic traversal over ther 'BitMatrix'.
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

instance Memoizable BitMatrix where
    memoize f (BitMatrix n bv) = memoize (f . BitMatrix n) bv
      
instance Memoizable BV where
    memoize f char = memoize (f . bitVec w) (nat char)
      where
        w = width char

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
    testBit      (BitMatrix _ b) i                   = b `testBit` i
    bit i                                            = BitMatrix 1 $ bit i
    bitSize                                          = fromMaybe 0 . bitSizeMaybe
    bitSizeMaybe (BitMatrix _ b)                     = bitSizeMaybe b
    isSigned     (BitMatrix _ b)                     = isSigned b
    popCount     (BitMatrix _ b)                     = popCount b

-- TODO: Don't know if this works. It's currently unused, I believe.
instance Arbitrary BitMatrix where
    arbitrary = do 
        alphLen  <- suchThat (arbitrary :: Gen Int) (\x -> 0 < x && x <= 52) 
        rowCount <- suchThat (arbitrary :: Gen Int) (> 0) 
        bitRows  <- vectorOf rowCount $ ((choose (1, 2 ^ alphLen - 1)) :: Gen Integer)
        pure . fromRows $ bitVec alphLen <$> bitRows

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
