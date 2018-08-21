module Main
  ( main
  , testSuite
  ) where

import           Data.Foldable
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import qualified TestSuite.ScriptTests as Script (testSuite)
import qualified TestSuite.GoldenTests as Golden (testSuite)


main :: IO ()
main
  = testSuite >>=
    defaultMainWithIngredients
    [rerunningTests defaultIngredients]


testSuite :: IO TestTree
testSuite
  = testGroup "Integration Test Suite"
  <$> sequenceA [ Script.testSuite, Golden.testSuite ]
