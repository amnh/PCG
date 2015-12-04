{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.Custom.Test
  ( testSuite 
  ) where
-- We only export the testSuite
-- This function can be combined with other testSuites definitions from other
-- Test modules to form a giant test suite to run

import Data.Either.Combinators
import Safe                    (readMay)
import Test.SmallCheck.Series  ()
import Test.Custom
import Test.Tasty              (TestTree,testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Custom

-- | Coalese the many TestTrees to a single TestTree
testSuite :: TestTree
testSuite = testGroup "Custom Parsec Combinator Tests" tests

-- | A list of all tests in represented as TestTrees
tests :: [TestTree]
tests = [ testGroup "Double Parsing" [decimalProperties]
        , testGroup "Inline Space Parsing" [inlineSpaceAssertions {- , inlineSpacesAssertions-}]
        ]

decimalProperties :: TestTree
decimalProperties = testGroup "Arbitrary Double Tests"
  [ testProperty "Injectivity"  decimalInjection
  , testProperty "Surjectivity" decimalSurjection
  ]

-- | Ensure that all Ints represented as Strings are correctly parsed as Ints.
--   The parser should never fail to parse a String representation of an Int.
-- NOTE: This doesn't work due to the strangeness of Decimal's precision
decimalSurjection :: Double -> Bool 
decimalSurjection x = Right x  == parse double "" (show x)

-- | Ensure that all Strings which can be `read` as an Int are parsed as Ints.
--   The parser should always fail to parse a String that cannot be read as an Int.
decimalInjection :: String -> Bool 
decimalInjection x =
  case readMay x :: Maybe Double of
    Nothing  -> True
    Just res -> parse (space *> double <* eof) "" x == Right res

inlineSpaceAssertions :: TestTree
inlineSpaceAssertions = testGroup "Inline Space Assertions" [validInlineSpace,invalidInlineSpace]
  where
    validInlineSpace = testGroup "Valid Inline Space"
      [ testCase "space" $ parseEquals inlineSpace " "  ' '
      , testCase "tab"   $ parseEquals inlineSpace "\t" '\t'
      , testCase "vtab"  $ parseEquals inlineSpace "\v" '\v'
      ]
    invalidInlineSpace = testGroup "Invalid Inline Space"
      [ testCase "newline"        $ parseFailure inlineSpace "\n"
      , testCase "caraige return" $ parseFailure inlineSpace "\r"
      ]
