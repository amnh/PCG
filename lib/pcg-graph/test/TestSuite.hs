module Main where

import Data.Graph.Test
import Test.Tasty
import Test.Tasty.Ingredients.Rerun (rerunningTests)

import Data.Graph.Type
import Data.Graph.Intermediate


main :: IO ()
main =
  do
    print "hoho"
    defaultMainWithIngredients
      [ rerunningTests defaultIngredients ]
      testSuite


exampleGraph :: Graph Identity () () String String
exampleGraph =
  Graph
  { leafReferences = fromList [
