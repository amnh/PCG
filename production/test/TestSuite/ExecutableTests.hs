module TestSuite.ExecutableTests
  ( main
  , testSuite
  ) where

import Test.Tasty

import qualified PCG.Evaluation.Test as Evaluation (testSuite)

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Executable Test Suite"
  [ Evaluation.testSuite
  ]
