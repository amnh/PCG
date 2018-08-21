module Main
  ( main
  , testSuite
  ) where

import           Data.Foldable
import           Test.Tasty
import qualified TestSuite.ScriptTests as Script (testSuite)


main :: IO ()
main = testSuite >>= defaultMain


testSuite :: IO TestTree
testSuite = testGroup "Integration Test Suite" <$> sequenceA [ Script.testSuite ]
