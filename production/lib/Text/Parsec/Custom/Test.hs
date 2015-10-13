{-# LANGUAGE FlexibleContexts #-}
module Text.Parsec.Custom.Test
  ( testSuite 
  ) where
-- We only export the testSuite
-- This function can be combined with other testSuites definitions from other
-- Test modules to form a giant test suite to run

import           Safe                         (readMay)
import           Test.SmallCheck.Series       ()
import           Test.Tasty                   (TestTree,testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Parsec                  (parse,eof,spaces)
import           Text.Parsec.Custom

-- | Coalese the many TestTrees to a single TestTree
testSuite :: TestTree
testSuite = testGroup "Custom Parsec Combinator Tests" tests

-- | A list of all tests in represented as TestTrees
tests :: [TestTree]
tests = [ testGroup "Integer Parsing" [integerProperties, validZeroTests, invalidPrefixes]
        , testGroup "Decimal Parsing" [decimalProperties]
        , testGroup "Inline Space Parsing" [inlineSpaceAssertions {- , inlineSpacesAssertions-}]
        ]



-- | TestTrees which originate from the QuickCheck framework test Properties
--   stocastically. QuickCheck will generate Arbitrary input values and ensure
--   that an certain relation holds. The following tests will stocastically
--   check that, for an Arbitrary value of `x`, the equality defined in terms
--   of `x` holds for all inputs provided. 
integerProperties :: TestTree
integerProperties = testGroup "Arbitrary Integral Tests"
  [ testProperty "Surjectivity" integerSurjection
  , testProperty "Injectivity"  integerInjection
  ]

-- | Ensure that all Ints represented as Strings are correctly parsed as Ints.
--   The parser should never fail to parse a String representation of an Int.
integerSurjection :: Int -> Bool 
integerSurjection x = Right x  == parse integer "" (show x)

-- | Ensure that all Strings which can be `read` as an Int are parsed as Ints.
--   The parser should always fail to parse a String that cannot be read as an Int.
integerInjection :: String -> Bool 
integerInjection x = (readMay x :: Maybe Int) == fromRight (parse (spaces *> integer <* eof) "" x)

-- | Since zero is the first symbol in the decimal base numebr system, it can 
--   be represent in many strange ways. We should properly handle these  
--   strange representations of zero.
validZeroTests :: TestTree
validZeroTests = testGroup "Strange Integral Zero Representations"
  [ testCase "Double Zero"       $ assert $ parse integer "" "00"       == Right 0
  , testCase "Plus Zero"         $ assert $ parse integer "" "+0"       == Right 0
  , testCase "Minus Zero"        $ assert $ parse integer "" "-0"       == Right 0
  , testCase "Plus Double Zero"  $ assert $ parse integer "" "+00"      == Right 0
  , testCase "Minus Double Zero" $ assert $ parse integer "" "-00"      == Right 0
  , testCase "Many Zeros"        $ assert $ parse integer "" "00000000" == Right 0
  ]

-- | The '+' & '-' prefixes on numbers should not be repeated. 
--   The parser should fail on consuming multiple of these prefixes.
invalidPrefixes :: TestTree
invalidPrefixes = testGroup "Invalid Integral Prefixes"
  [ testCase "Double Plus"  $ assert $ (not . isRight) $ parse integer "" "++0"
  , testCase "Double Minus" $ assert $ (not . isRight) $ parse integer "" "--0"
  ]

decimalProperties :: TestTree
decimalProperties = testGroup "Arbitrary Decimal Tests"
  [ testProperty "Injectivity"  decimalInjection
--  , testProperty "Surjectivity" decimalSurjection
  ]

{-
-- | Ensure that all Ints represented as Strings are correctly parsed as Ints.
--   The parser should never fail to parse a String representation of an Int.
-- NOTE: This doesn't work due to the strangeness of Decimal's precision
 decimalSurjection :: Double -> Bool 
 decimalSurjection x = Right x  == parse decimal "" (show x)
-}

-- | Ensure that all Strings which can be `read` as an Int are parsed as Ints.
--   The parser should always fail to parse a String that cannot be read as an Int.
decimalInjection :: String -> Bool 
decimalInjection x = (readMay x :: Maybe Double) == fromRight (parse (spaces *> decimal <* eof) "" x)

inlineSpaceAssertions :: TestTree
inlineSpaceAssertions = testGroup "Inline Space Assertions" [validInlineSpace,invalidInlineSpace]
  where
    validInlineSpace = testGroup "Valid Inline Space"
      [ testCase "space"       $ assert $ parse inlineSpace "" " "  == Right ' '
      , testCase "tab"         $ assert $ parse inlineSpace "" "\t" == Right '\t'
      , testCase "vtab"        $ assert $ parse inlineSpace "" "\v" == Right '\v'
      ]
    invalidInlineSpace = testGroup "Invalid Inline Space"
      [ testCase "newline"        $ assert $ isLeft $ parse inlineSpace "" "\n"
      , testCase "caraige return" $ assert $ isLeft $ parse inlineSpace "" "\r"
      ]


-- | Helper functions

isLeft :: Either a b -> Bool
isLeft = not . isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight _         = Nothing
  
