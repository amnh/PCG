module TestSuite.ExecutableTests
  ( main
  , testSuite
  ) where

import Test.Tasty

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Executable Test Suite" []
