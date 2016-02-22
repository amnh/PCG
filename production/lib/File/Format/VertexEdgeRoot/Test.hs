{-# LANGUAGE FlexibleContexts #-}

module File.Format.VertexEdgeRoot.Test
  ( testSuite
  ) where

import Data.List                         (intercalate)
import File.Format.VertexEdgeRoot.Parser
import File.Format.VertexEdgeRoot.Converter
import Test.Custom                       (parseEquals,parseFailure,parseSuccess, convertEquals)
import Test.Tasty                        (TestTree,testGroup)
import Test.Tasty.HUnit
import Text.Megaparsec                   (eof, parse)
import qualified Data.IntMap as IM
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Bio.Phylogeny.Graph        hiding (EdgeInfo)
import Bio.Phylogeny.Tree.Node

testSuite :: TestTree
testSuite = testGroup "VER Format"
  [ testGroup "VER Combinators"
      [vertexSetType',vertexLabelDefinition',unlabeledVertexSetDefinition'
      ,labeledVertexSetDefinition',edgeDefinition',edgeSetDefinition']
  , testGroup "VER Parser" 
      [verStreamParser']
  , testGroup "VER Converter"
      [verSimpleConvert]
  ]

validSetLabels :: [(VertexSetType,String)]
validSetLabels =
  [ (Roots,"RootSet")
  , (Vertices,"VertexSet")
  , (Roots,"rOotsEt")
  ]

invalidSetLabels :: [String]
invalidSetLabels =
  [ ""          -- empty string
  , "Roots"     -- Not right
  , "Verticies" -- Neither is this
  ]

vertexSetType' :: TestTree
vertexSetType' = testGroup "vertexSetType" [validLines,invalidLines]
  where
    validLines        = testGroup "Valid set labels"   $ success <$> validSetLabels
    invalidLines      = testGroup "Invalid set labels" $ failure <$> invalidSetLabels
    success (res,str) = testCase (show str) $ parseEquals  (vertexSetType <* eof) str res
    failure str       = testCase (show str) $ parseFailure (vertexSetType <* eof) str

validVertexLabels :: [String]
validVertexLabels =
  [ "A"
  , "B"
  , "C"
  , "The_Node"
  , "McLovin'"
  , "arr[i]"
  , "\\Gamma"
  , "P&Q"
  ]

invalidVertexLabels :: [String]
invalidVertexLabels =
  [ ""
  , " blanks within "
  , "tab\tforward"
  , "new\nline"
  , "such,comma,overload,"
  , "many{braces}"
  ]

vertexLabelDefinition' :: TestTree
vertexLabelDefinition' = testGroup "vertexLabelDefinition" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid vertex labels"   $ success <$> validVertexLabels
    invalidLines = testGroup "Invalid vertex labels" $ failure <$> invalidVertexLabels
    success str  = testCase (show str) $ parseEquals  (vertexLabelDefinition <* eof) str str
    failure str  = testCase (show str) $ parseFailure (vertexLabelDefinition <* eof) str

unlabeledVertexSetDefinition' :: TestTree
unlabeledVertexSetDefinition' = testGroup "unlabeledVertexSetDefinition" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid unlabeled vertex set" . pure  $ success validVertexSet
    invalidLines = testGroup "Invalid unlabeled vertex sets"      $ failure <$> invalidVertexSets
    success str  = testCase (show str) $ parseSuccess (unlabeledVertexSetDefinition <* eof) str
    failure str  = testCase (show str) $ parseFailure (unlabeledVertexSetDefinition <* eof) str
    invalidVertexSets =
      [ "{no,trailing,brace"
      , "no,leading,brace}"
      , "{}"
      , "{duplicate,duplicate,entries}"
      ]

validVertexSet :: String
validVertexSet = (\x -> "{"++x++"}") $ intercalate "," validVertexLabels

labeledVertexSetDefinition' :: TestTree
labeledVertexSetDefinition' = testGroup "labeledVertexSetDefinition" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid labeled vertex set"    $ success <$> validLabeledVertexSets
    invalidLines = testGroup "Invalid labeled vertex sets" $ failure <$> invalidLabeledVertexSets
    success str  = testCase (show str) $ parseSuccess (labeledVertexSetDefinition <* eof) str
    failure str  = testCase (show str) $ parseFailure (labeledVertexSetDefinition <* eof) str
    validLabeledVertexSets =
      [ label++"="++set | (_,label) <-  validSetLabels 
                        , set       <- [validVertexSet]
      ]
    invalidLabeledVertexSets = 
      [ "={empty,label}"
      , "RootSet{no,equals,sign}"
      ]

edgeDefinition' :: TestTree
edgeDefinition' = testGroup "edgeDefinition" [validLines,invalidLines]
  where
    validLines        = testGroup "Valid edge"   $ success <$> validEdges
    invalidLines      = testGroup "Invalid edge" $ failure <$> invalidEdges
    success (res,str) = testCase (show str) $ parseEquals  (edgeDefinition <* eof) str res
    failure str       = testCase (show str) $ parseFailure (edgeDefinition <* eof) str

validEdges :: [(EdgeInfo,String)]
validEdges =
  [ (EdgeInfo ("a","b")   Nothing   , "(a,b)"            )
  , (EdgeInfo ("a","b")   Nothing   , " ( a , b ) "      )
  , (EdgeInfo ("a","b") $ Just 42.0 , "(a,b):42"         )
  , (EdgeInfo ("a","b") $ Just 1.337, "(a,b):1.337"      )
  , (EdgeInfo ("a","b") $ Just 0.07 , " ( a , b ) : 0.07")
  ]

invalidEdges :: [String]
invalidEdges =
  [ "(a,b"
  , "a,b)"
  , "(a b)"
  , "(a,b)42"
  ]

edgeSetDefinition' :: TestTree
edgeSetDefinition' = testGroup "edgeSetDefinition" [validSets,invalidSets]
  where
    validSets   = testGroup "Valid edge sets"   $ success <$> validEdgeSets
    invalidSets = testGroup "Invalid edge sets" $ failure <$> invalidEdgeSets
    success str = testCase (show str) $ parseSuccess (edgeSetDefinition <* eof) str
    failure str = testCase (show str) $ parseFailure (edgeSetDefinition <* eof) str

validEdgeSets :: [String]
validEdgeSets =
  [ "EdgeSet={(a,b)}"
  , "eDgEsEt={(a,b)}"
  , "{(a,b),(c,d)}"
  , "EdgeSet={}" -- It doesn't *have* to have edges
  ]

invalidEdgeSets :: [String]
invalidEdgeSets =
  [ "{(a,b),(a,b)}"
  , "{(a,b),(b,a)}"
  , "{(a,b):1,(a,b):2}" -- still invalid
  , "{(a,a)}" -- cannot be connected to yourself
  ]

verStreamParser' :: TestTree
verStreamParser' = testGroup "verStreamParser" [valid,invalid]
  where
    valid        = testGroup "Valid VER definitions"   $ success <$> validVerDefs
    invalid      = testGroup "Invalid VER definitions" $ failure <$> invalidVerDefs
    success str  = testCase (show str) $ parseSuccess (verStreamParser <* eof) str
    failure str  = testCase (show str) $ parseFailure (verStreamParser <* eof) str
    validVerDefs =
      ["{a,b,c}{(a,b),(a,c)}{a}"
      ]
    invalidVerDefs =
      [ "{a,b,c}{(a,b),(a,c),(b,c)}{a}" -- contains a cycle
      , "{a,b,c}{(a,b),(a,c)}{a,c}"     -- root nodes are connected
      ]

verSimpleConvert :: TestTree
verSimpleConvert = testGroup "verSimpleConvert" [single]
  where
    single = testCase "A one node input makes a one node graph" (convertEquals (verStreamParser <* eof) "RootSet={a}VertexSet={a}EdgeSet={}" singleGraph convert)
    singleGraph = Graph [Tree (IM.singleton 0 "a") (HM.singleton "a" mempty) mempty (V.singleton $ Node 0 True True [] [] mempty mempty mempty mempty mempty mempty 0) (V.singleton $ EdgeSet mempty mempty) 0]

