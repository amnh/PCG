{-# LANGUAGE FlexibleInstances #-}

module Data.BitMatrix.Test
  ( testSuite
  ) where

import Data.BitMatrix
import Data.BitVector hiding (foldr)
import Data.Foldable
import Test.Tasty
import Test.Tasty.QuickCheck


newtype DependantFromRowsParameters
      = DependantFromRowsParameters
      { getParameters :: (Int, Int, [BitVector])
      } deriving (Eq, Show)


instance Arbitrary DependantFromRowsParameters where

    arbitrary = do
        rowCount   <- getPositive <$> arbitrary
        colCount   <- getPositive <$> arbitrary
        let bvGen  =  fromBits    <$> vectorOf colCount (arbitrary :: Gen Bool)
        bitVectors <- vectorOf rowCount bvGen
        pure $ DependantFromRowsParameters (rowCount, colCount, bitVectors)


testSuite :: TestTree
testSuite = testGroup "BitMatrix tests"
    [ testRowsFromRows
    , testBitMatrix
    , testFromRows
    , testConsistentIndexing
    ]


-- Just to make sure that rows . fromRows == id.
testRowsFromRows :: TestTree
testRowsFromRows = testProperty "rows . fromRows === id" f
  where
    f :: DependantFromRowsParameters -> Property
    f params = rows (fromRows bvList) === bvList
      where
        (_, _, bvList) = getParameters params


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
