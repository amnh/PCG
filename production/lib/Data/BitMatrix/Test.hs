{-# LANGUAGE FlexibleInstances #-}

module Data.BitMatrix.Test
  ( testSuite
  ) where

import Data.BitMatrix
import Data.Bits
import Data.BitVector hiding (foldr)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "BitMatrix tests" [testRowsFromRows, testBitMatrixFn, testFromRowsFn, testConsistentIndexing]

-- Just to make sure that rows . fromRows == id.
testRowsFromRows :: TestTree
testRowsFromRows = testProperty "rows $ fromRows x == id" f
    where
        f :: (Positive Int, Positive Int, [BitVector]) -> Bool
        f (_, _, bvList) = rows (fromRows bvList) == bvList


-- both sets of tests on generating functions rely on BitMatrix functions rows, numRows, numCols
testBitMatrixFn :: TestTree
testBitMatrixFn = testGroup "bitMatrix generating fn" [ testValue
                                                      , testWidth
                                                      , testHeight
                                                      , testRow
                                                      ]
    where
        testValue = testProperty "Internal BitVector value is correct." f
        -- Note that it only tests on a single input function
            where
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = testBM == controlBM
                    where
                        testBM = Data.BitVector.concat $ rows (bitMatrix numChars alphLen $ const True)
                        controlBM = bitVec (alphLen * numChars) (2 ^ (alphLen * numChars) - 1)
                        numChars  = getPositive rowCt
                        alphLen   = getPositive colCt
        testWidth = testProperty "Number of columns is correct." f
        -- Note that it only tests on a single input function
            where
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = alphLen == numCols testBM
                    where
                        testBM   = bitMatrix numChars alphLen $ const True
                        numChars = getPositive rowCt
                        alphLen  = getPositive colCt
        testHeight = testProperty "Number of rows is correct." f
        -- Note that it only tests on a single input function
            where
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = numChars == numRows testBM
                    where
                        testBM   = bitMatrix numChars alphLen $ const True
                        numChars = getPositive rowCt
                        alphLen  = getPositive colCt

testFromRowsFn :: TestTree
testFromRowsFn = testGroup "fromRows generating fn" [ testValue
                                                      , testWidth
                                                      , testHeight]
    where
        testValue = testProperty "Internal BitVector value is correct." f
        -- Note that it only tests on a single input function
        -- Also, relies on `rows` fn.
            where
                f :: (Positive Int, Positive Int, [BitVector]) -> Bool
                f (_, _, bvs) = testBM == controlBM
                    where
                        testBM    = mconcat . rows $ fromRows bvs
                        controlBM = mconcat bvs
                        
        testWidth = testProperty "Number of columns is correct." f
        -- Note that it only tests on a single input function
            where
                f :: (Positive Int, Positive Int, [BitVector]) -> Bool
                f (_, colCt, bvs) = getPositive colCt == numCols testBM
                    where
                        testBM = fromRows bvs
                        
        testHeight = testProperty "Number of rows is correct." f
        -- Note that it only tests on a single input function
            where
                f :: (Positive Int, Positive Int, [BitVector]) -> Bool
                f (rowCt, _, bvs) = getPositive rowCt == numRows testBM
                    where
                        testBM = fromRows bvs

testRow :: TestTree
testRow = testProperty "row returns correct value" f
    where
        f :: (Positive Int, Positive Int, [BitVector]) -> Bool
        f (_, _, bvList) = retVal 
            where 
                -- at each item in the list of bvs, test it againts what ought to be at that index, and accumulate
                (retVal, _) = foldr (\bv (bool, i) -> ((bv == row testBM i) && bool, i + 1)) (True, 0) bvList
                testBM      = fromRows bvList

testConsistentIndexing :: TestTree
testConsistentIndexing = testProperty "Indexing and generation consistency" f
    where
        f :: Blind ((Int,Int) -> Bool) -> Gen Bool
        f b =
            do
                rows <- getPositive <$> (arbitrary :: Gen (Positive Int))
                cols <- getPositive <$> (arbitrary :: Gen (Positive Int))
                let bm = bitMatrix rows cols g
                let indices = [ (x,y) | x <- [0..rows-1], y <- [0..cols-1] ]
                pure $ all (\x -> g x == bm `isSet` x) indices
            where
                g = getBlind b


instance Arbitrary (Positive Int, Positive Int, [BitVector]) where
  arbitrary = do
    rowCount   <- getPositive <$> arbitrary
    colCount   <- getPositive <$> arbitrary
    let bvGen  =  fromBits <$> vectorOf colCount (arbitrary :: Gen Bool)
    bitVectors <- vectorOf rowCount bvGen
    pure (Positive rowCount, Positive colCount, bitVectors)

