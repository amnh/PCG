{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Data.TCM.Test
  ( testSuite
  ) where

import           Data.TCM
import           Test.HUnit.Custom (assertException)
import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC hiding (generate)
import           Data.MonoTraversable
import           Data.Word
import           Data.Bifunctor (bimap)
import           Test.QuickCheck (Positive(getPositive), (===), Testable(property))


testSuite :: TestTree
testSuite = testGroup "TCM Tests"
    [ testExampleCases
    , testPropertyCases
    ]

testPropertyCases :: TestTree
testPropertyCases = testGroup "Invariant Properties"
    [ diagnoseTcmCases
    ]

testExampleCases :: TestTree
testExampleCases = testGroup "Example Cases for Data.TCM"
    [ documentationCases
    ]

-- Generate cases for TcmStructure diagnosis

structureType :: TCM -> TCMStructure
structureType = tcmStructure . diagnoseTcm

diagnoseTcmCases :: TestTree
diagnoseTcmCases = testGroup "Example cases for TCMDiagnosis"
    [ QC.testProperty
        "generate k \\(i,j) -> n * i + m * j is non-symmetric for n \\= m"
        nonSymmetricProp
    , QC.testProperty
        "generate k \\(i,j) -> a * (i * j) + b * (i + j) + c is at worst symmetric"
        symmetricProp
    , QC.testProperty
        "generate k \\(i,j) -> (max i j) - (min i j) is Additive"
        additiveProp
    , QC.testProperty
        "generate k \\(i,j) -> if i == j then 0 else 1 is NonAdditive"
        nonAdditiveProp
    ]
  where
    nonSymmetricProp :: (Positive Int, Positive  Int, Positive  Int) -> Property
    nonSymmetricProp (k', n', m') =
      let
        k = getPositive k'
        n = getPositive n'
        m = getPositive m'
      in
        if n /= m then
            structureType (generate (k + 1) $ \(i,j) -> n * i + m * j)
            === NonSymmetric
        else property True


    symmetricProp :: (Positive Int, Positive  Int, Positive Int) -> Bool
    symmetricProp (k', a', b') =
      let
        k = getPositive k'
        a = getPositive a'
        b = getPositive b'
      in
        structureType (generate (k + 1) $ \(i,j) -> a * (i * j) + b * (i + j))
        /= NonSymmetric

    additiveProp :: Positive Int -> Property
    additiveProp k' =
      let
        k = getPositive k'
      in
        structureType (generate (k + 1) $ \(i,j) -> max i j - min i j)
        === Additive

    nonAdditiveProp :: Positive Int -> Property
    nonAdditiveProp k' =
      let
        k = getPositive k'
      in
        structureType (generate (k + 2) $ \ ((i,j) :: (Int, Int))-> if i == j then 0 else 1)
        === NonAdditive         -- |
                                -- |
                                -- | -- Ensure matrix is at least 3 x 3 as k = 2 case is both
                                -- | -- additive and nonadditive.

-- Generate cases for TCMDiagnosis factoring

factoringDiagnosisCases :: TestTree
factoringDiagnosisCases = testGroup "Example cases for factoredTcm and factoredWeight"
    [ QC.testProperty "(omap (* weight) factoredTcm) === tcm" factorProp
    ]
  where
    factorProp :: TCM -> Property
    factorProp tcm =
      let
        TCMDiagnosis{..} = diagnoseTcm tcm
        weight = fromIntegral factoredWeight
      in
        omap (* (weight :: Word32)) factoredTcm
        === tcm

-- Examples from documentation

-- Helper function to extract list of elements and size of TCM

elementsAndSize :: TCM -> ([Word32], Int)
elementsAndSize = bimap otoList size . \a -> (a,a)

documentationCases :: TestTree
documentationCases = testGroup "Example cases in documentation"
    [ fromCases
    , generateCases
    ]


fromCases :: TestTree
fromCases = testGroup "Cases of from{List,Cols,Rows} function"
    [ HU.testCase
        (unlines
        ["fromList [1..9] =="
        ,"           TCM: 3 x 3"
        , "            1 2 3"
        , "            4 5 6"
        , "            7 8 9"
        ])
        fromListEx
    , HU.testCase "fromList [] raises exception"      $ assertException fromList []
    , HU.testCase "fromList [42] raises exception"    $ assertException fromList [42]
    , HU.testCase "fromList [1..12] raises exception" $ assertException fromList [1..12]
    , HU.testCase
        (unlines
        [ "fromCols [[1,2,3],[4,5,6],[7,8,9]] =="
        , "          TCM: 3 x 3"
        , "            1 4 7"
        , "            2 5 8"
        , "            3 6 9"
        ])
        fromColsEx
    , HU.testCase
        (unlines
        [ "fromRows [[1,2,3],[4,5,6],[7,8,9]] =="
        , "          TCM: 3 x 3"
        , "            1 2 3"
        , "            4 5 6"
        , "            7 8 9"
        ])
        fromRowsEx
    ]
  where
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

generateCases :: TestTree
generateCases = testGroup "Cases of generate function"
    [ HU.testCase
        (unlines
        [ "generate 5 $ const 5 =="
        , "          TCM: 5 x 5"
        , "            5 5 5 5 5"
        , "            5 5 5 5 5"
        , "            5 5 5 5 5"
        , "            5 5 5 5 5"
        , "            5 5 5 5 5"
        ])
        generateCase1
    , HU.testCase
        (unlines
        [ "generate 4 $ \\(i,j) -> abs (i - j) =="
        , "          TCM: 4 x 4"
        , "            0 1 2 3"
        , "            1 0 1 2"
        , "            2 1 0 1"
        , "            3 2 1 0"
        ])
        generateCase2
    , HU.testCase
        (unlines
        [ "generate 8 $ \\(i,j) -> if i == j || i + j == 6 then 0 else 1 =="
        , "          TCM: 8 x 8"
        , "            0 1 1 1 1 1 0 1"
        , "            1 0 1 1 1 0 1 1"
        , "            1 1 1 1 0 1 1 1"
        , "            1 1 1 0 1 1 1 1"
        , "            1 1 0 1 0 1 1 1"
        , "            1 0 1 1 1 0 1 1"
        , "            0 1 1 1 1 1 0 1"
        , "            1 1 1 1 1 1 1 0"
        ])
        generateCase3
    ]
  where
    generateCase1 :: Assertion
    generateCase1 =
      let tcm = generate 5 (const 5 :: (Int, Int) -> Int) in
        elementsAndSize tcm
        @?= (replicate 25 5, 5)

    generateCase2 :: Assertion
    generateCase2 =
      let tcm = generate 4 $ \(i,j) -> abs (i - j) in
        elementsAndSize tcm
        @?= ([ 0, 1, 2, 3
             , 1, 0, 1, 2
             , 2, 1, 0, 1
             , 3, 2, 1, 0] , 4)

    generateCase3 :: Assertion
    generateCase3 =
      let tcm = generate 8 $ \(i,j) -> if i == j || i + j == 6 then 0 else 1 in
        elementsAndSize tcm
        @?= ([ 0, 1, 1, 1, 1, 1, 0, 1
             , 1, 0, 1, 1, 1, 0, 1, 1
             , 1, 1, 0, 1, 0, 1, 1, 1
             , 1, 1, 1, 0, 1, 1, 1, 1
             , 1, 1, 0, 1, 0, 1, 1, 1
             , 1, 0, 1, 1, 1, 0, 1, 1
             , 0, 1, 1, 1, 1, 1, 0, 1
             , 1, 1, 1, 1, 1, 1, 1, 0], 8)