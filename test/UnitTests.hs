module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import qualified TestSuite.ExecutableTests    as Executable (testSuite)
import qualified TestSuite.LibraryTests       as Library (testSuite)


main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


testSuite :: TestTree
testSuite = testGroup "Unit Test Suite" [ Library.testSuite, Executable.testSuite ]
