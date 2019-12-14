module Main where

import qualified Control.Evaluation.Test      as Evaluation
import qualified System.ErrorPhase.Test       as ErrorPhase
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)



main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite

testSuite :: TestTree
testSuite = testGroup "Evaluation Tests"
    [ ErrorPhase.testSuite
    , Evaluation.testSuite
    ]
