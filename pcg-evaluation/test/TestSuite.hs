module Main where

import qualified Evaluation.Test              as Evaluation
import qualified Monad.Logger.Test            as Logger
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
    , Logger.testSuite
    ]
