{-# LANGUAGE FlexibleInstances #-}

module Data.BitVector.Test
  ( testSuite
  ) where

import Data.Bits
import Data.BitVector.Normal
import Data.Foldable
import Data.MonoTraversable
import Test.Tasty
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ otoListTest
    , finiteBitsTests
    ]


finiteBitsTests :: TestTree
finiteBitsTests = testGroup "FiniteBits instance consistency"
    [ testProperty "fromEnum . dimension === finiteBitSize" finiteBitSizeIsDimension 
    , testProperty "length . toBits === finiteBitSize" finiteBitSizeIsBitLength 
    , localOption (QuickCheckTests  100000)
       $ testProperty "length . takeWhile not === countLeadingZeros . fromBits" countLeadingZeroAndFromBits
    , localOption (QuickCheckTests  100000)
       $ testProperty "length . takeWhile not . toBits === countLeadingZeros" countLeadingZeroAndToBits
    , testProperty "length . takeWhile not . reverse === countTrailingZeros . fromBits" countTrailingZeroAndFromBits
    , testProperty "length . takeWhile not . reverse . toBits === countTrailingZeros" countTrailingZeroAndToBits
    ]
  where
    finiteBitSizeIsDimension :: BitVector -> Property
    finiteBitSizeIsDimension bv =
      (fromEnum . dimension) bv === finiteBitSize bv
      
    finiteBitSizeIsBitLength :: BitVector -> Property
    finiteBitSizeIsBitLength bv =
      (length . toBits) bv === finiteBitSize bv
      
    countLeadingZeroAndFromBits :: [Bool] -> Property
    countLeadingZeroAndFromBits bs =
      (length . takeWhile not) bs === (countLeadingZeros . fromBits) bs

    countLeadingZeroAndToBits :: BitVector -> Property
    countLeadingZeroAndToBits bv =
      (length . takeWhile not . toBits) bv === countLeadingZeros bv

    countTrailingZeroAndFromBits :: [Bool] -> Property
    countTrailingZeroAndFromBits bs = 
      (length . takeWhile not . reverse) bs === (countTrailingZeros . fromBits) bs
      
    countTrailingZeroAndToBits :: BitVector -> Property
    countTrailingZeroAndToBits bv =
      (length . takeWhile not . reverse . toBits) bv === countTrailingZeros bv
    

otoListTest :: TestTree
otoListTest = testProperty "otoList === toBits" f
  where
    f :: BitVector -> Property
    f bv = otoList bv === toBits bv


{-
-- both sets of tests on generating functions rely on BitMatrix functions rows, numRows, numCols
testBitMatrix :: TestTree
testBitMatrix = testGroup "bitMatrix generating function"
    [ testValue
    , testWidth
    , testHeight
    , testRow
    ]
  where
    testValue = testProperty "Internal BitVector value is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = testBM === controlBM
          where
            testBM = Data.BitVector.concat $ rows (bitMatrix numChars alphLen $ const True)
            controlBM = bitVec (alphLen * numChars) (2 ^ (alphLen * numChars) - 1 :: Integer)
            numChars  = getPositive rowCt
            alphLen   = getPositive colCt

    testWidth = testProperty "Number of columns is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = expectedCols === numCols testBM
          where
            (_, expectedCols, testBM) = constructMatrixFromPositives rowCt colCt

    testHeight = testProperty "Number of rows is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = expectedRows === numRows testBM
          where
            (expectedRows, _, testBM) = constructMatrixFromPositives rowCt colCt


constructMatrixFromPositives
  :: Positive Int          -- ^ Number of rows
  -> Positive Int          -- ^ Number of columns
  -> (Int, Int, BitMatrix) -- ^ Extracted Int values and resulting zero matrix
constructMatrixFromPositives rowCt colCt = (numRows', numCols', bitMatrix numRows' numCols' $ const False)
  where
    numRows' = getPositive rowCt
    numCols' = getPositive colCt


testFromRows :: TestTree
testFromRows = testGroup "Construction using fromRows"
    [ testValue
    , testWidth
    , testHeight
    ]
  where
    testValue = testProperty "Internal BitVector value is correct" f
    -- Relies on `rows` function.
      where
        f :: DependantFromRowsParameters -> Bool
        f params = testBM == controlBM
          where
            (_, _, bvs) = getParameters params
            testBM      = mconcat . rows $ fromRows bvs
            controlBM   = mconcat bvs
                        
    testWidth = testProperty "Number of columns is correct" f
      where
        f :: DependantFromRowsParameters -> Bool
        f params = colCt == numCols testBM
          where
            (_, colCt, bvs) = getParameters params
            testBM = fromRows bvs
                        
    testHeight = testProperty "Number of rows is correct" f
      where
        f :: DependantFromRowsParameters -> Bool
        f params = rowCt == numRows testBM
          where
            (rowCt, _, bvs) = getParameters params
            testBM = fromRows bvs


testRow :: TestTree
testRow = testProperty "row returns correct value" f
  where
    f :: DependantFromRowsParameters -> Bool
    f params = retVal 
      where 
        -- at each item in the list of bvs, test it againts what ought to be at that index, and accumulate
        (retVal, _)    = foldl' (\(bool, i) bv -> ((bv == row testBM i) && bool, i + 1)) (True, 0) bvList
        (_, _, bvList) = getParameters params
        testBM         = fromRows bvList



testConsistentIndexing :: TestTree
testConsistentIndexing = testProperty "Indexing and generation consistency" f
  where
    f :: Blind ((Int,Int) -> Bool) -> Gen Bool
    f blindFunction = do
        rowCount    <- getPositive <$> (arbitrary :: Gen (Positive Int))
        colCount    <- getPositive <$> (arbitrary :: Gen (Positive Int))
        let bm      =  bitMatrix rowCount colCount g
        let indices =  [ (i,j) | i <- [0..rowCount-1], j <- [0..colCount-1] ]
        -- The generating function at a given index is the same as
        -- the bit tester at that index anfter bitMatrix generation.
        pure $ all (\x -> g x == bm `isSet` x) indices
      where
        g = getBlind blindFunction


testIsZeroMatrix :: TestTree
testIsZeroMatrix = testProperty "isZeroMatrix bm <===> ∀ i, not (bm `isSet` i)" f
  where
    -- The generating function always returns false if and only if
    -- the BitMatrix contains only zeros.
    f :: BitMatrix -> Bool
    f bm = all (not . isSet bm) indices == isZeroMatrix bm
      where
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]


testRowsToList :: TestTree
testRowsToList = testProperty "otoList === rows" f
  where
    f :: BitMatrix -> Property
    f bm = otoList bm === rows bm


testRowCountConsistency :: TestTree
testRowCountConsistency = testProperty "numRows === length . rows" f
  where
    f :: BitMatrix -> Property
    f bm = numRows bm === length (rows bm)


testRowIndexConsistency :: TestTree
testRowIndexConsistency = testProperty "∀ i, (`row` i) === (! i) . rows" f
  where
    f :: BitMatrix -> Property
    f bm = conjoin $ g <$> [ 0 .. numRows bm - 1 ]
      where
        g i = bm `row` i === rows bm !! i


testExpandRows :: TestTree
testExpandRows = testProperty "toBits . expandRows === fmap (isSet bm)" f
  where
    f :: BitMatrix -> Property
    f bm = (reverse . toBits . expandRows) bm === (isSet bm <$> indices)
      where
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]


testFactorRows :: TestTree
testFactorRows = testProperty "toBits === fmap (isSet bm) . factorRows n" f
  where
    f :: FactoredBitVector -> Property
    f input = (reverse . toBits) bv === (isSet bm <$> indices)
      where
        bm = factorRows colCount bv
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]
        (_, colCount, bv) = getFactoredBitVector input

          
testExpandFactorIdentity :: TestTree
testExpandFactorIdentity = testProperty "factorRows numCols . expandRows === id" f
  where
    f :: BitMatrix -> Property
    f bm = factorRows (numCols bm) (expandRows bm) === bm

-}
