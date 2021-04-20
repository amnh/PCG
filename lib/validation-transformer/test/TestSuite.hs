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

module Main where

import qualified Control.Monad.Trans.Validation.Test as ValidationT
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun        (rerunningTests)


-- |
-- The entry point for the test suite.
main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


-- |
-- Complete test-suite for 'Control.Monad.Trans.Validation.ValidationT'.
testSuite :: TestTree
testSuite = testGroup "ValidationT test-suite"
    [ ValidationT.testSuite
    ]
