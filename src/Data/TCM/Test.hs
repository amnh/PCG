module Data.TCM.Test
  ( testSuite
  ) where

import           Data.TCM
import           Test.HUnit.Custom
import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC hiding (generate)
import           Test.HUnit.Custom (assertException)
import           Data.MonoTraversable
import           Data.Word
import           Data.Bifunctor (bimap)


testSuite :: TestTree
testSuite = testGroup "TCM Tests"
    [ testPropertyCases
    , testExampleCases
    ]

testPropertyCases :: TestTree
testPropertyCases = testGroup "Invariant Properties"
    [
    ]

testExampleCases :: TestTree
testExampleCases = testGroup "Example Cases for Data.TCM"
    [ documentationCases
    ]

indexProperties :: TestTree
indexProperties = testGroup "Properties of index function:"
    [
    ]


indexCases :: TestTree
indexCases = testGroup "Example Cases for index function"
    [ HU.testCase "case" ex1
    ]
  where
    ex1 :: Assertion
    ex1 = undefined


-- Examples from documentation

documentationCases :: TestTree
documentationCases = testGroup "Example cases in documentation"
    [ HU.testCase
        (unlines
        ["fromList [1..9] ="
        ,"TCM: 3 x 3"
        , "  1 2 3"
        , "  4 5 6"
        , "  7 8 9"
        ])
        fromListEx
    , HU.testCase "fromList [] raises exception" $ assertException fromList []
    , HU.testCase "fromList [42] raises exception" $ assertException fromList [42]
    , HU.testCase "fromList [1..12] raises exception" $ assertException fromList [1..12]
    , HU.testCase
        (unlines
        [ "fromCols [[1,2,3],[4,5,6],[7,8,9]]"
        , "TCM: 3 x 3"
        , "  1 4 7"
        , "  2 5 8"
        , "  3 6 9"
        ])
        fromColsEx
    , HU.testCase
        (unlines
        [ "fromRows [[1,2,3],[4,5,6],[7,8,9]]"
        , "TCM: 3 x 3"
        , "  1 2 3"
        , "  4 5 6"
        , "  7 8 9"
        ])
        fromRowsEx
    , HU.testCase
        (unlines
        [ "generate 5 $ const 5"
        , "TCM: 5 x 5"
        , "  5 5 5 5 5"
        , "  5 5 5 5 5"
        , "  5 5 5 5 5"
        , "  5 5 5 5 5"
        , "  5 5 5 5 5"
        ])
        generateCase1
    ]
  where
    elementsAndSize :: TCM -> ([Word32], Int)
    elementsAndSize = (bimap otoList size) . (\a -> (a,a))

    fromListEx :: Assertion
    fromListEx =
      let tcm = snd . fromList $ [1..9] in
        elementsAndSize tcm
        @?= ([1..9] , 3)

    fromColsEx :: Assertion
    fromColsEx =
      let tcm = snd .  fromCols $ [[1,2,3],[4,5,6],[7,8,9]] in
        elementsAndSize tcm
        @?= ([1, 4, 7, 2, 5, 8, 3, 6, 9], 3)

    fromRowsEx :: Assertion
    fromRowsEx =
      let tcm = snd . fromRows $  [[1,2,3],[4,5,6],[7,8,9]] in
        elementsAndSize tcm
        @?= ([1..9], 3)

    generateCase1 :: Assertion
    generateCase1 =
      let tcm = generate 5 (const 5 :: (Int, Int) -> Int) in
        elementsAndSize tcm
        @?= (replicate 25 5, 5)
