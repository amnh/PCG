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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE TypeFamilies       #-}


module Data.BitMatrix.Internal where

import Control.DeepSeq
import Data.Binary
import Data.Bits
import Data.BitVector.LittleEndian
import Data.BitVector.LittleEndian.Instances ()
import Data.Foldable
import Data.List.Utility           (equalityOf, invariantTransformation)
import Data.MonoTraversable
import Data.Ord
import GHC.Generics
import Test.QuickCheck             hiding ((.&.))
import TextShow                    (TextShow (showb), singleton, unlinesB, unwordsB)


-- |
-- A data structure for storing a two dimensional array of bits.
-- Exposes row based monomorphic mapping & folding.
--
-- It is important to note the endianness of 'BitMatrix'.
-- The bit at position @(0,0)@ is displayed in the upper left hand corner when
-- the 'BitMatrix' is shown.
-- The bit at position @(i,x)@ will be of less significance than position @(i+1,x)@,
-- for the resulting xth 'BitVector' row when calling 'rows' on a 'BitMatrix'.
{-
-- The bit at position @(x,i)@ will be of less significance than position @(x,i+1)@,
-- for the resulting xth 'BitVector' column when calling 'cols' on a 'BitMatrix'.
-}
data  BitMatrix
    = BitMatrix {-# UNPACK #-} !Int {-# UNPACK #-} !BitVector
    deriving anyclass (Binary, NFData)
    deriving stock    (Eq, Generic)


-- |
-- The row based element for monomorphic maps & folds.
type instance Element BitMatrix = BitVector


-- |
-- Resulting matricies will have at /least/ one row and one column.
instance Arbitrary BitMatrix where

    arbitrary = do
        colCount <- choose (1, 20) :: Gen Int
        rowCount <- choose (1, 20) :: Gen Int
        let rVal =  choose (0, 2 ^ colCount - 1) :: Gen Integer
        bitRows  <- vectorOf rowCount rVal
        pure . fromRows $ fromNumber (toEnum colCount) <$> bitRows


-- |
-- Performs a row-wise monomporphic fold over ther 'BitMatrix'.
instance MonoFoldable BitMatrix where

    -- |
    -- Map each element of a monomorphic container to a 'Monoid'
    -- and combine the results.
    {-# INLINE ofoldMap #-}
    ofoldMap f = ofoldr (mappend . f) mempty

    -- |
    -- Right-associative fold of a monomorphic container.
    {-# INLINE ofoldr #-}
    ofoldr f e = foldr f e . rows

    -- |
    -- Strict left-associative fold of a monomorphic container.
    {-# INLINE ofoldl' #-}
    ofoldl' f e = foldl' f e . rows

    -- |
    -- Right-associative fold of a monomorphic container with no base element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of
    -- this function./
    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = foldr1 f . rows

    -- |
    -- Strict left-associative fold of a monomorphic container with no base
    -- element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version
    -- of this function./
    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = foldl1 f . rows

    {-# INLINE onull #-}
    onull (BitMatrix 0 _) = True
    onull _               = False

    {-# INLINE olength #-}
    olength = fromEnum . numRows

    headEx bm@(BitMatrix c bv)
      | dimension bv < n = error $ "call to BitMatrix.headEx with: " <> show bm
      | otherwise        = (0, n - 1) `subRange` bv
      where
        n = toEnum c

    lastEx bm@(BitMatrix c bv)
      | d < n     = error $ "call to BitMatrix.lastEx with: " <> show bm
      | otherwise = (d - n, d - 1) `subRange` bv
      where
        d = dimension bv
        n = toEnum c


instance MonoFunctor BitMatrix where

    omap f bm =
        case invariantTransformation finiteBitSize rows' of
            Just i  -> BitMatrix i $ fold rows'
            Nothing -> error "The mapping function over the bit matrix did not return *all* bit vectors of equal length."
      where
        rows' = f <$> rows bm


instance MonoTraversable BitMatrix where

    otraverse f = fmap correction . traverse f . rows
      where
        correction xs =
          case invariantTransformation finiteBitSize xs of
            Just i  -> BitMatrix i $ fold xs
            Nothing -> error "The mapping function over the bit matrix did not return *all* bit vectors of equal length."


instance Ord BitMatrix where

  compare lhs rhs =
     case comparing numRows lhs rhs of
       EQ ->
           case comparing numCols lhs rhs of
             EQ -> comparing expandRows lhs rhs
             v  -> v
       v  -> v


instance Show BitMatrix where

    show bm = headerLine <> matrixLines
      where
        renderRow   = foldr (\e acc -> (if e then '1' else '0') : acc) "" . toBits
        matrixLines = unlines $ renderRow <$> rows bm
        headerLine  = '\n' : unwords
                    [ "BitMatrix:"
                    , show $ numRows bm
                    , "x"
                    , show $ numCols bm
                    , "\n"
                    ]

instance TextShow BitMatrix where

    showb bm = headerLine <> matrixLines
      where
        renderRow   = foldMap (\b -> if b then singleton '1' else singleton '0') . toBits
        matrixLines = unlinesB $ renderRow <$> rows bm
        headerLine  = singleton '\n' <>
                    unwordsB
                    [ "BitMatrix:"
                    , showb $ numRows bm
                    , "x"
                    , showb $ numCols bm
                    , "\n"
                    ]


-- |
-- \( \mathcal{O} \left( m * n \right) \)
--
-- A generating function for a 'BitMatrix'. Efficiently constructs a
-- 'BitMatrix' of the specified dimensions with each bit defined by the result
-- of the supplied function.
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
--
bitMatrix :: Word                   -- ^ Number of rows in the BitMatrix.
          -> Word                   -- ^ Number of columns in the BitMatrix.
          -> ((Word, Word) -> Bool) -- ^ Function to determine if a given index has a set bit.
          -> BitMatrix
bitMatrix m n f =
  case errorMsg of
    Just msg -> error msg
    Nothing  -> BitMatrix (fromEnum n) . fromNumber (m * n) . snd . foldl' g initialAccumulator $ [(i,j) | i <- [0..m-1], j <- [0..n-1]]
  where
    initialAccumulator :: (Integer, Integer)
    initialAccumulator = (1,0)

    g (!shiftRegister, !summation) i
      | f i       = (shiftRegister `shiftL` 1, shiftRegister + summation)
      | otherwise = (shiftRegister `shiftL` 1,                 summation)

    errorMsg
      | m <  0 && n <  0 = Just $ unwords [errorPrefix, errorRowCount, ". Also, ", errorColCount] <> "."
      | m <  0           = Just $ unwords [errorPrefix, errorRowCount] <> "."
      | n <  0           = Just $ unwords [errorPrefix, errorColCount] <> "."
      | m == 0 && n /= 0 = Just $ unwords [errorPrefix, errorZeroRows, errorZeroSuffix] <> "."
      | m /= 0 && n == 0 = Just $ unwords [errorPrefix, errorZeroCols, errorZeroSuffix] <> "."
      | otherwise        = Nothing
      where
        errorPrefix     = fold ["The call to bitMatrix ", show m, " ", show n, " f is malformed,"]
        errorRowCount   = fold ["the number of rows, "   , show m, ", is a negative number"]
        errorColCount   = fold ["the number of columns, ", show n, ", is a negative number"]
        errorZeroRows   = fold ["the number of rows was 0 but the number of columns, ", show n, ", was positive."]
        errorZeroCols   = fold ["the number of columns was 0 but the number of rows, ", show m, ", was positive."]
        errorZeroSuffix = "To construct the empty matrix, both rows and columns must be zero"


{-
col :: BitMatrix -> Int -> BitVector
col = undefined -- bit twiddle or math
-}


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Extracts a bitvector with all cells concatenated in a row-major manner.
{-# INLINE expandRows #-}
expandRows :: BitMatrix -> BitVector
expandRows (BitMatrix _ bv) = bv


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Constructs a 'BitMatrix' from a 'BitVector' with all cells wrapped in a
-- row-major manner.
{-# INLINE factorRows #-}
factorRows :: Word -> BitVector -> BitMatrix
factorRows n bv
  | len `mod` n == 0 = BitMatrix (fromEnum n) bv
  | otherwise        = error erroMsg
  where
    len = dimension bv
    erroMsg = fold
        [ "The supplied BitVector length ("
        , show len
        , ") cannot be evenly divided by the supplied column count ("
        , show n
        , ")."
        ]


-- |
-- \( \mathcal{O} \left( m \right) \)
--
-- Construct a 'BitMatrix' from a list of rows.
fromRows :: Foldable t => t BitVector -> BitMatrix
fromRows xs
  | equalityOf finiteBitSize xs = result
  | otherwise                   = error "fromRows: All the rows did not have the same width!"
  where
    result =
        case toList xs of
          []  -> BitMatrix 0 zeroBits
          y:_ -> BitMatrix (finiteBitSize y) $ fold xs


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Test if a bit is set at the given indices.
isSet :: BitMatrix -> (Word, Word) -> Bool
isSet (BitMatrix n bv) (i, j) = bv `testBit` (n * fromEnum i + fromEnum j)


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Determines if there are no set bits in the 'BitMatrix'
isZeroMatrix :: BitMatrix -> Bool
isZeroMatrix (BitMatrix _ bv) = (toUnsignedNumber bv :: Integer) == 0


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- The number of columns in the 'BitMatrix'
numCols :: BitMatrix -> Word
numCols (BitMatrix n _) = toEnum n


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- The number of rows in the 'BitMatrix'
numRows :: BitMatrix -> Word
numRows (BitMatrix n bv)
  | n == 0    = 0
  | otherwise = dimension bv `div` toEnum n


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Retreives a single row of the 'BitMatrix'. Allows for unsafe indexing.
row :: BitMatrix -> Word -> BitVector
row bm@(BitMatrix nCols bv) i
  | i < nRows = (lower, upper) `subRange` bv
  | otherwise = error errorMsg
  where
    -- It couldn't be more clear
    upper    = toEnum nCols * (i + 1) - 1
    lower    = upper - toEnum nCols + 1
    nRows    = numRows bm
    errorMsg = unwords ["Index", show i, "is outside the range", rangeStr]
    rangeStr = fold ["[0..", show nRows, "]."]


-- |
-- \( \mathcal{O} \left( m \right) \)
--
-- The rows of the 'BitMatrix'
rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix nCols bv)
    | nRows == 0 || nCols == 0 = []
    | nRows == 1               = [bv]
    | otherwise                = go nRows initAcc
    where
      nRows   = numRows bm
      dim     = toEnum nCols
      mask    = (2^dim) - 1 :: Integer

      initAcc = (toUnsignedNumber bv, []) :: (Integer, [BitVector])

      go 0 (   _, xs) = reverse xs
      go n (!val, xs) = go (n-1) (val `shiftR` nCols, fromNumber dim (val .&. mask) : xs)
