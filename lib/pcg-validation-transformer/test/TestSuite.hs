module Main where

import qualified Control.Monad.Trans.Validation.Test as ValidationT
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun        (rerunningTests)



main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite

testSuite :: TestTree
testSuite = testGroup "ValidationT test-suite"
    [ ValidationT.testSuite
    ]
