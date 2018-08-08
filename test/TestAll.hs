module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import qualified TestSuite.ExecutableTests as Executable (testSuite)
import qualified TestSuite.LibraryTests    as Library (testSuite)

main :: IO ()
main = defaultMain Library.testSuite


testSuite :: TestTree
testSuite = testGroup "Unit Test Suite" [ Library.testSuite, Executable.testSuite ]
