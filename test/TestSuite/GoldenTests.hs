module TestSuite.GoldenTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

testSuite :: IO TestTree
testSuite = do
  pure
    $ testGroup "Golden tests for [...]"
    [
    ]
