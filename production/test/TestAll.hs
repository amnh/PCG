module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import qualified TestSuite.LibraryTests    as Library    (testSuite)
import qualified TestSuite.ExecutableTests as Executable (testSuite)
import qualified TestSuite.GeneratedTests  as Generated  (testSuite)

main :: IO ()
main = do 
   dynamicTests <- Generated.testSuite
   defaultMain $ testGroup "Complete Test Suite" [ {- Library.testSuite, Executable.testSuite, -} dynamicTests ]

testSuite :: TestTree
testSuite = testGroup "Unit Test Suite" [ Library.testSuite, Executable.testSuite ]
