module Main
  ( main
  ) where

import qualified Analysis.Clustering.Test                                    as Clustering
import qualified Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test as Pairwise
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun                                (rerunningTests)



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
