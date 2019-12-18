{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Functor.Const
import Data.Graph.Indices
import Data.Graph.Intermediate
import Data.Graph.Internal
import Data.Graph.NodeContext
--import Data.Graph.Test
import Data.Graph.Type
import Data.Pair.Strict
import Data.Vector
--import Test.Tasty.Ingredients.Rerun (rerunningTests)


main :: IO ()
main =
  do
    putStrLn ""
    putStrLn (showGraphAsRoseForest exampleGraph1)
    putStrLn ""
    putStrLn (showGraphAsRoseForest exampleGraph2)
    putStrLn ""
    putStrLn (showGraphAsRoseForest exampleGraph3)
    putStrLn ""
    putStrLn (showGraphAsRoseForest intBinTree)
    putStrLn ""
    putStrLn (showGraphAsRoseForest intBinTree2)
--    defaultMainWithIngredients
--      [ rerunningTests defaultIngredients ]
--      testSuite


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
              (childInfo LeafTag 0 () :!: childInfo LeafTag 1 ())
          , treeIndexData
              (Id "tree2")
              (tagValue RootTag 0)
              (childInfo LeafTag 2 () :!: childInfo LeafTag 3 ())
          ]
  , networkReferences = mempty
  , rootReferences
      = fromList
          [ rootIndexData
              (Id "root")
              (Right $ childInfo TreeTag 0 () :!: childInfo TreeTag 1 ())
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
              (childInfo LeafTag 0 () :!: childInfo LeafTag 1 ())
          , treeIndexData
              (Id "tree2")
              (tagValue RootTag 0)
              (childInfo TreeTag 2 () :!: childInfo TreeTag 3 ())
          , treeIndexData
              (Id "tree3")
              (tagValue RootTag 0)
              (childInfo LeafTag 2  () :!: childInfo LeafTag 3 ())
          , treeIndexData
              (Id "tree4")
              (tagValue RootTag 0)
              (childInfo LeafTag 4 () :!: childInfo LeafTag 5 ())

          ]
  , networkReferences = mempty
  , rootReferences
      = fromList
          [ rootIndexData
              (Id "root")
              (Right $ childInfo TreeTag 0 () :!: childInfo TreeTag 1 ())
          ]
  , cachedData = ()
  }

exampleGraph3 :: Graph Id () () String String
exampleGraph3 =
  Graph
  { leafReferences
      = fromList
          [ leafIndexData "leaf1"   $ tagValue TreeTag 0
          , leafIndexData "leaf2"   $ tagValue TreeTag 0
          , leafIndexData "leaf3"   $ tagValue TreeTag 1
          , leafIndexData "leaf4/5" $ tagValue TreeTag 1
          , leafIndexData "leaf6"   $ tagValue TreeTag 1
          ]
  , treeReferences
      = fromList
          [ treeIndexData
              (Id "tree1")
              (tagValue RootTag 0)
              (childInfo LeafTag 0 () :!: childInfo LeafTag 1 ())
          , treeIndexData
              (Id "tree2")
              (tagValue RootTag 0)
              (childInfo TreeTag 2 () :!: childInfo TreeTag 3 ())
          , treeIndexData
              (Id "tree3")
              (tagValue RootTag 0)
              (childInfo NetworkTag 0  () :!: childInfo LeafTag 2 ())
          , treeIndexData
              (Id "tree4")
              (tagValue RootTag 0)
              (childInfo NetworkTag 0 () :!: childInfo LeafTag 4 ())

          ]
  , networkReferences
      = fromList
        [ networkIndexData
            (Id "#net1")
            (tagValue TreeTag 2 :!: tagValue TreeTag 3)
            (childInfo LeafTag 3 ())
        ]
  , rootReferences
      = fromList
          [ rootIndexData
              (Id "root")
              (Right $ childInfo TreeTag 0 () :!: childInfo TreeTag 1 ())
          ]
  , cachedData = ()
  }


intBinTree :: Graph (Const ()) () () Int Int
intBinTree = Graph
  { leafReferences
      = fromList
          [ leafIndexData 5 (tagValue TreeTag 0)
          , leafIndexData 2 (tagValue TreeTag 0)
          , leafIndexData 3 (tagValue TreeTag 1)
          , leafIndexData 9 (tagValue TreeTag 1)
          ]
  , treeReferences
      = fromList
          [ treeIndexData
              (Const ())
              (tagValue RootTag 0)
              (childInfo LeafTag 0 () :!: childInfo LeafTag 1 ())
          , treeIndexData
              (Const ())
              (tagValue RootTag 0)
              (childInfo LeafTag 2 () :!: childInfo LeafTag 3 ())
          ]
  , rootReferences
      = fromList
          [ rootIndexData
              (Const ())
              (Right $ childInfo TreeTag 0 () :!: childInfo TreeTag 1 ())
          ]
  , networkReferences = mempty
  , cachedData = ()
  }

intBinTree2 :: Graph [] () () Int Int
intBinTree2 = postorder id f intBinTree
  where
    f :: [Int] -> [Int] -> [Int]
    f [a] [b] = [a,b, a + b]
    f xs ys   = (+) <$> xs <*> ys




newtype Id a = Id {runId :: a}
  deriving newtype (Show)
