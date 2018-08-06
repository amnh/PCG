module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import qualified TestSuite.LibraryTests    as Library    (testSuite)
import qualified TestSuite.ExecutableTests as Executable (testSuite)
import           Test.Tasty.Ingredients.Rerun (rerunningTests)

main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


testSuite :: TestTree
testSuite = testGroup "Unit Test Suite" [ Library.testSuite, Executable.testSuite ]
