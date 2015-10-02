module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import qualified TestSuite.LibraryTests    as Library    (testSuite)
import qualified TestSuite.ExecutableTests as Executable (testSuite)

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Complete Test Suite" [ Library.testSuite, Executable.testSuite ]
