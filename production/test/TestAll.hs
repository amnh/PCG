module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import qualified TestSuite.LibraryTests    as Library    (testSuite)
import qualified TestSuite.ExecutableTests as Executable (testSuite)

main :: IO ()
main = defaultMain Library.testSuite


testSuite :: TestTree
testSuite = testGroup "Unit Test Suite" [ Library.testSuite, Executable.testSuite ]
