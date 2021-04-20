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
  ) where

import qualified Analysis.Clustering.Test                                    as Clustering
import qualified Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test as Pairwise
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun                                (rerunningTests)


-- |
-- Entry point for the test-suite of the "analysis" library.
main :: IO ()
main =
    defaultMainWithIngredients
    [ rerunningTests defaultIngredients ]
    testSuite


testSuite :: TestTree
testSuite = testGroup
              "Analysis Test Suite"
              [ Pairwise.testSuite
              , Clustering.testSuite
              ]
