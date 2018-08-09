{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.VertexEdgeRoot.Test
  ( testSuite
  ) where

--import           Bio.PhyloGraph.Node
import Data.List                         (intercalate)
import File.Format.VertexEdgeRoot.Parser
import Test.Custom.Parse                 (parseEquals, parseFailure,
                                          parseSuccess)
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.Megaparsec                   (eof)


testSuite :: TestTree
testSuite = testGroup "VER Format"
    [ testGroup "VER Combinators"
        [ vertexSetType'
        , vertexLabelDefinition'
        , unlabeledVertexSetDefinition'
        , labeledVertexSetDefinition'
        , edgeDefinition'
        , edgeSetDefinition'
        ]
    , testGroup "VER Parser"
        [ verStreamParser' ]
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
validVertexSet = (\x -> "{"<>x<>"}") $ intercalate "," validVertexLabels


labeledVertexSetDefinition' :: TestTree
labeledVertexSetDefinition' = testGroup "labeledVertexSetDefinition" [validLines,invalidLines]
  where
    validLines   = testGroup "Valid labeled vertex set"    $ success <$> validLabeledVertexSets
    invalidLines = testGroup "Invalid labeled vertex sets" $ failure <$> invalidLabeledVertexSets
    success str  = testCase (show str) $ parseSuccess (labeledVertexSetDefinition <* eof) str
    failure str  = testCase (show str) $ parseFailure (labeledVertexSetDefinition <* eof) str
    validLabeledVertexSets =
        [ label<>"="<>set | (_,label) <-  validSetLabels
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
    [ (EdgeInfo "a" "b"   Nothing   , "(a,b)"            )
    , (EdgeInfo "a" "b"   Nothing   , " ( a , b ) "      )
    , (EdgeInfo "a" "b" $ Just 42.0 , "(a,b):42"         )
    , (EdgeInfo "a" "b" $ Just 1.337, "(a,b):1.337"      )
    , (EdgeInfo "a" "b" $ Just 0.07 , " ( a , b ) : 0.07")
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
    [ "{(a,b),(a,b)}"     -- No dupes allowed
    , "{(a,b):1,(a,b):2}" -- Still an invalid dupe
    , "{(a,b),(b,a)}"     -- No bidirectional edges allowed
    , "{(a,a)}"           -- Cannot be connected to yourself
    ]


verStreamParser' :: TestTree
verStreamParser' = testGroup "verStreamParser" [valid,invalid]
  where
    valid        = testGroup "Valid VER definitions"   $ success <$> validVerDefs
    invalid      = testGroup "Invalid VER definitions" $ failure <$> invalidVerDefs
    success str  = testCase (show str) $ parseSuccess (verStreamParser <* eof) str
    failure str  = testCase (show str) $ parseFailure (verStreamParser <* eof) str
    validVerDefs =
        [ -- Single simple tree
          "{a,b,c}{(a,b),(a,c)}{a}"
          -- Disjoint simple trees
        , "{a,b,c,x,y,z}{(a,b),(a,c),(x,y),(x,z)}{a,x}"
          -- Single network with a "network" edge
        , "{a,b,c,d,e,f}{(a,b),(a,c),(b,d),(b,e),(c,e),(c,f)}{a}"
          -- Single network with a "component" edge
        , "{a,b,c,d,e,f,g}{(a,b),(a,c),(b,d),(b,e),(c,e),(c,f),(g,c)}{a,g}"
          -- dijoint network with a "component" edge & simple tree
        , "{a,b,c,d,e,f,g,h,i}{(a,b),(a,c),(b,d),(b,e),(c,e),(c,f),(g,c),(h,i)}{a,g,h}"
        ]
    invalidVerDefs =
        [  -- Contains a cycle
          "{a,b,c,d}{(a,b),(b,c),(c,d),(d,b)}{a}"
           -- Root nodes has a parent
        , "{a,b,c}{(a,b),(c,a)}{a}"
        ]
