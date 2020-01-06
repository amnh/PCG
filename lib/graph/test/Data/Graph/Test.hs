module Data.Graph.Test
  ( testSuite
  ) where

-- import Data.Foldable
-- import Data.Functor.Identity
--import Data.Graph
import Test.Tasty
-- import Test.Tasty.HUnit      as HU
-- import Test.Tasty.QuickCheck as QC


-- type TestGraph = Graph Identity () () Int Int


testSuite :: TestTree
testSuite = testGroup "Graph Tests"
    [ testPropertyCases
    , testExampleCases
    ]


testPropertyCases :: TestTree
testPropertyCases = testGroup "Invariant properties"
    [
    ]


testExampleCases :: TestTree
testExampleCases = testGroup "Example cases for Data.Graph"
    [
    ]


{-
toDoProperties :: TestTree
toDoProperties = testGroup "Properties of TODO"
    [ QC.testProperty "This property holds"
                      property
    ]
  where
    property :: TestGraph  -> Bool
    property = const True
-}



-- Cases for unit tests

{-
exampleBalancedbinaryTree :: TestGraph
exampleBalancedbinaryTree = undefined
-}


{-
balancedBinaryTreeCases :: TestTree
balancedBinaryTreeCases =
  testGroup
    (fold
     ["Cases for TODO:\n"
     , "TODO"
     ]
    )
      [ HU.testCase "Unit test text" assertion
      ]
  where
    assertion :: Assertion
    assertion = True @?= True
-}

