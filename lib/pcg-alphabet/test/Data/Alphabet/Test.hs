module Data.Alphabet.Test
  ( testSuite
  ) where

import Data.Alphabet
import Data.Foldable
import Data.List             (nubBy)
import Test.Tasty
import Test.Tasty.HUnit      as HU
import Test.Tasty.QuickCheck as QC


testSuite :: TestTree
testSuite = testGroup "Alphabet Tests"
    [ testPropertyCases
    , testExampleCases
    ]


testPropertyCases :: TestTree
testPropertyCases = testGroup "Invariant properties"
    [ alphabetStateNamesProperties
    , alphabetSymbolsProperties
    , gapSymbolProperties
    , truncateAtSymbolProperties
    , truncateAtMaxSymbolProperties
    ]


testExampleCases :: TestTree
testExampleCases = testGroup "Example cases for Data.Alphabet"
    [ alphabetDNACases
    ]


alphabetStateNamesProperties :: TestTree
alphabetStateNamesProperties = testGroup "Properties of alphabetStateNames"
    [ QC.testProperty "The list of state names is always empty if none are supplied"
                      noStateNames
    ]
  where
    noStateNames :: [String] -> Bool
    noStateNames = null . alphabetStateNames . fromSymbols


alphabetSymbolsProperties :: TestTree
alphabetSymbolsProperties = testGroup "Properties of alphabetSymbols"
    [ QC.testProperty "The list of symbols is the same if we forget the state names"
                      forgetStateNames
    ]
  where
    forgetStateNames :: [(String, String)] -> Property
    forgetStateNames strs' =
      let strs =
            nubBy (\p q -> fst p == fst q) strs' in
              (alphabetSymbols . fromSymbolsWithStateNames $ strs)
              === (alphabetSymbols . fromSymbols . fmap fst $ strs)


gapSymbolProperties :: TestTree
gapSymbolProperties = testGroup "Properties of gapSymbol"
    [ QC.testProperty "The gap symbol is always \"-\"" constGapSymbol
    ]
  where
    constGapSymbol :: [(String, String)] -> Property
    constGapSymbol = (=== "-") . gapSymbol . fromSymbolsWithStateNames


truncateAtSymbolProperties :: TestTree
truncateAtSymbolProperties = testGroup "Properties of truncateAtSymbol"
    [ QC.testProperty
      "TruncateAtSymbol y (fromSymbols (xs ++ [y] ++ ys)) == fromSymbols (xs ++ [y])"
      splitOrderedList
    ]
  where
    splitOrderedList :: (NonNegative Int, [String]) -> Property
    splitOrderedList (NonNegative n, strs') =
      let strs = toList $ fromSymbols strs'
      in  case splitAt n strs of
            (_,   []) -> property True
            (xs, y:_) -> (truncateAtSymbol y . fromSymbols $ strs) === fromSymbols (xs <> [y])


truncateAtMaxSymbolProperties :: TestTree
truncateAtMaxSymbolProperties = testGroup "Properties of truncateAtMaxSymbol"
    [ QC.testProperty
        "truncateAtMaxSymbol of the input returns the original alphabet"
        truncatePreserve
    ]
  where
    truncatePreserve :: [String] -> Property
    truncatePreserve strs = let alph = fromSymbols strs in
        truncateAtMaxSymbol strs alph === alph


-- Cases for unit tests

alphabetDNAString :: [(String, String)]
alphabetDNAString =
  [ ("A", "adenine")
  , ("C", "cytosine")
  , ("G", "guanine")
  , ("T", "thymine")
  ]


{-
alphabetDNAText :: [(T.Text, T.Text)]
alphabetDNAText =
  fmap (\(s1, s2) -> (T.pack s1, T.pack s2)) alphabetDNAString
-}


alphabetDNA :: Alphabet String
alphabetDNA = fromSymbolsWithStateNames alphabetDNAString


alphabetDNACases :: TestTree
alphabetDNACases =
  testGroup
    (unlines
     ["Cases for DNA alphabet given by:"
     , "     A   adenine"
     , "     C   cytosine"
     , "     G   guanine"
     , "     T   thymine"
     ]
    )
      [ HU.testCase "The symbols are A, C, G, T and -" symbols1
      , HU.testCase "The state names are adenine, cytosine, guanine, thymine and -" states1
      ]
  where
    symbols1 :: Assertion
    symbols1 = alphabetSymbols alphabetDNA @?= ["A", "C", "G", "T", "-"]

    states1 :: Assertion
    states1 = alphabetStateNames alphabetDNA
             @?= ["adenine", "cytosine", "guanine", "thymine", "-"]
