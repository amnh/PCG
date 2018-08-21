module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun           (rerunningTests)
import qualified TestSuite.ScriptTests        as Script (testSuite)


main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


testSuite :: TestTree
testSuite = testGroup "Integration Test Suite" [ Script.testSuite ]
