module Main where

import Alphabet.Test
import Test.Tasty
import Test.Tasty.Ingredients.Rerun          (rerunningTests)



main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite
