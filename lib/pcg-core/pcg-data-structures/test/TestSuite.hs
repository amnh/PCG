module Main
  ( main
  , testSuite
  ) where

import qualified Bio.Character.Encodable.Dynamic.Test as DynamicChar
import qualified Bio.Character.Encodable.Static.Test  as StaticChar
import qualified Bio.Graph.ReferenceDAG.Test          as ReferenceDAG
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)


main :: IO ()
main = defaultMainWithIngredients
    [ rerunningTests defaultIngredients ]
    testSuite


testSuite :: TestTree
testSuite = testGroup "PCG core library test suite"
    [ DynamicChar.testSuite
    , StaticChar.testSuite
    , ReferenceDAG.testSuite
    ]
