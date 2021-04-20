------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main
  ( main
  , testSuite
  ) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import qualified TestSuite.GoldenTests        as Golden (testSuite)
import qualified TestSuite.ScriptTests        as Script


-- |
-- The entry point for the integration test suite.
main :: IO ()
main =
    testSuite >>=
    defaultMainWithIngredients
    [rerunningTests defaultIngredients]


-- |
-- Collection of /all/ integration tests.
testSuite :: IO TestTree
testSuite = testGroup "Integration Test Suite" . (\x -> scriptTests <> [x])
          <$> Golden.testSuite
  where
    scriptTests =
      [ Script.failureTestSuite
      , Script.commandTestSuite
      , Script.costTestSuite
      ]
