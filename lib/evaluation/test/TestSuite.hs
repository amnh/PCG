-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Tests for the evaluation sub-library.
--
-----------------------------------------------------------------------------

module Main where

import qualified Control.Evaluation.Test      as Evaluation
import qualified System.ErrorPhase.Test       as ErrorPhase
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)


-- |
-- Executable entry point for the evaluation sub-library's test suite.
main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


-- |
-- Test suite for the evaluation sub-library.
testSuite :: TestTree
testSuite = testGroup "Evaluation Tests"
    [ ErrorPhase.testSuite
    , Evaluation.testSuite
    ]
