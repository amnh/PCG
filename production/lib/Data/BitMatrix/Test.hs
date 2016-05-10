{-# LANGUAGE FlexibleInstances #-}

module Data.BitMatrix.Test
  ( testSuite
  ) where

import Data.BitMatrix
import Data.Bits
import Data.BitVector 
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "BitMatrix tests" [testBitMatrixFn, testFromRowsFn]

-- both sets of tests on generating functions rely on BitMatrix functions rows, numRows, numCols
testBitMatrixFn :: TestTree
testBitMatrixFn = testGroup "bitMatrix generating fn" [ testValue
                                                       , testWidth
                                                       , testHeight]
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
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = testBM == controlBM
                    where
                        testBM = fromRows bitsList
                        controlBM = Prelude.concat bitsList
                        numChars  = getPositive rowCt
                        alphLen   = getPositive colCt
                        boolList  = do pure $ take alphLen <$> infiniteListOf (arbitrary :: Bool)
                        bitsList  = take numChars $ repeat boolList
        testWidth = testProperty "Number of columns is correct." f
        -- Note that it only tests on a single input function
            where
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = alphLen == numCols testBM
                    where
                        testBM = bitMatrix numChars alphLen $ const True
                        numChars  = getPositive rowCt
                        alphLen   = getPositive colCt
        testHeight = testProperty "Number of rows is correct." f
        -- Note that it only tests on a single input function
            where
                f :: Positive Int -> Positive Int -> Bool
                f rowCt colCt = numChars == numRows testBM
                    where
                        testBM = bitMatrix numChars alphLen $ const True
                        numChars  = getPositive rowCt
                        alphLen   = getPositive colCt
