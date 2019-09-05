{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Graph.Test
import Test.Tasty
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Data.Vector
import Data.Pair.Strict

import Data.Graph.Type
import Data.Graph.Intermediate
import Data.Graph.Indices
import Data.Graph.NodeContext


main :: IO ()
main =
  do
    putStrLn ""
    putStrLn (showGraphAsRoseForest exampleGraph1)
    putStrLn ""
    putStrLn (showGraphAsRoseForest exampleGraph2)
    defaultMainWithIngredients
      [ rerunningTests defaultIngredients ]
      testSuite


exampleGraph1 :: Graph Id () () String String
exampleGraph1 =
  Graph
  { leafReferences
      = fromList
          [ leafIndexData "leaf1" (tagValue TreeTag 0)
          , leafIndexData "leaf2" (tagValue TreeTag 0)
          , leafIndexData "leaf3" (tagValue TreeTag 1)
          , leafIndexData "leaf4" (tagValue TreeTag 1)
          ]
  , treeReferences
      = fromList
          [ treeIndexData
              (Id "tree1")
              (tagValue RootTag 0)
              (tagValue LeafTag 0 :!: tagValue LeafTag 1)
          , treeIndexData
              (Id "tree2")
              (tagValue RootTag 0)
              (tagValue LeafTag 2 :!: tagValue LeafTag 3)
          ]
  , networkReferences = mempty
  , rootReferences
      = fromList
          [ rootIndexData
              (Id "root")
              (Right $ (tagValue TreeTag 0) :!: (tagValue TreeTag 1))
          ]
  , cachedData = ()
  }


exampleGraph2 :: Graph Id () () String String
exampleGraph2 =
  Graph
  { leafReferences
      = fromList
          [ leafIndexData "leaf1" (tagValue TreeTag 0)
          , leafIndexData "leaf2" (tagValue TreeTag 0)
          , leafIndexData "leaf3" (tagValue TreeTag 1)
          , leafIndexData "leaf4" (tagValue TreeTag 1)
          , leafIndexData "leaf5" (tagValue TreeTag 1)
          , leafIndexData "leaf6" (tagValue TreeTag 1)
          ]
  , treeReferences
      = fromList
          [ treeIndexData
              (Id "tree1")
              (tagValue RootTag 0)
              (tagValue LeafTag 0 :!: tagValue LeafTag 1)
          , treeIndexData
              (Id "tree2")
              (tagValue RootTag 0)
              (tagValue TreeTag 2 :!: tagValue TreeTag 3)
          , treeIndexData
              (Id "tree3")
              (tagValue RootTag 0)
              (tagValue LeafTag 2 :!: tagValue LeafTag 3)
          , treeIndexData
              (Id "tree4")
              (tagValue RootTag 0)
              (tagValue LeafTag 4 :!: tagValue LeafTag 5)

          ]
  , networkReferences = mempty
  , rootReferences
      = fromList
          [ rootIndexData
              (Id "root")
              (Right $ (tagValue TreeTag 0) :!: (tagValue TreeTag 1))
          ]
  , cachedData = ()
  }

newtype Id a = Id {runId :: a}
  deriving newtype (Show)
