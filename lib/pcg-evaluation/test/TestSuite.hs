module Main where

import qualified Control.Evaluation.Test      as Evaluation
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)



main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite

testSuite :: TestTree
testSuite = testGroup "Evaluation Tests"
    [ Evaluation.testSuite
    ]
