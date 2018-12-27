{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Graph.ReferenceDAG.Test
  ( testSuite
  ) where

import           Bio.Graph.ReferenceDAG.Internal
import           Data.Set                        as Set
import qualified Data.Vector                     as V
import           Test.Tasty
import           Test.Tasty.HUnit                as HU


testSuite :: TestTree
testSuite = testGroup "ReferenceDAG Tests"
    [ testExampleCases
    ]

testExampleCases :: TestTree
testExampleCases = testGroup "Example Cases for Bio.Graph.ReferenceDAG"
    [ candidateNetworkEdgesCases
    ]


renderExampleTree :: String
renderExampleTree = unlines
                      [ "                       6"
                      , "                     /   \\"
                      , "                    4     5"
                      , "                   /  \\  /  \\"
                      , "                  0   1  2   3"
                      ]

exampleTreeCandidateNetworkEdges :: Set ((Int, Int), (Int, Int))
exampleTreeCandidateNetworkEdges
  = [
      ((4,0), (5,2)), ((4,0), (5,3))
    , ((4,1), (5,2)), ((4,1), (5,3))
    , ((5,2), (4,0)), ((5,2), (4,1))
    , ((5,3), (4,0)), ((5,3), (4,1))
    ]

renderExampleTreeCandidateNetworkEdges :: String
renderExampleTreeCandidateNetworkEdges
    = unlines
    [ "              Candidate Edge Pairs: "
    , "              {"
    , "                     ((4,0), (5,2)), ((4,0), (5,3)), ((4,0), (6,5))"
    , "                     ((4,1), (5,2)), ((4,1), (5,3)), ((4,1), (6,5))"
    , "                     ((5,2), (4,0)), ((5,2), (4,1)), ((5,2), (6,4))"
    , "                     ((5,3), (4,0)), ((5,3), (4,1)), ((5,3), (6,4))"
    , "                     ((6,4), (5,2)), ((6,4), (5,3))"
    , "                     ((6,5), (4,0)), ((6,5), (4,1))"
    , "              }"
    ]

exampleTree :: ReferenceDAG () () ()
exampleTree = ReferenceDAG{..}
  where
    references = V.generate 7 referenceFn
    rootRefs   = [6]
    graphData  = zeroCostGraphData

    referenceFn :: Int -> IndexData () ()
    referenceFn =
      \case
        0 -> IndexData () [4] []
        1 -> IndexData () [4] []
        2 -> IndexData () [5] []
        3 -> IndexData () [5] []
        4 -> IndexData () [6] [(0, ()), (1, ())]
        5 -> IndexData () [6] [(2, ()), (3, ())]
        6 -> IndexData () []  [(4, ()), (5, ())]
        _ -> error "Access value outside of exampleTree"



renderExampleNetwork :: String
renderExampleNetwork = unlines
                         [ "                         8"
                         , "                      /     \\"
                         , "                     6       7"
                         , "                   /  \\     /  \\"
                         , "                  4    \\   /    5"
                         , "                         3     / \\"
                         , "                         |    1   2"
                         , "                         0"
                         ]

exampleNetworkCandidateNetworkEdges :: Set ((Int, Int), (Int, Int))
exampleNetworkCandidateNetworkEdges =
    [ ((6, 4), (5, 1)), ((6, 4), (5, 2))
    ]


renderExampleNetworkCandidateNetworkEdges :: String
renderExampleNetworkCandidateNetworkEdges
    = unlines
    [ "             Candidate Edge Pairs: "
    , "             {"
    , "                  ((6, 4), (5, 1)), ((6, 4), (5, 2))"
    , "             }"
    ]


exampleNetwork :: ReferenceDAG () () ()
exampleNetwork = ReferenceDAG{..}
  where
    references = V.generate 9 referenceFn
    rootRefs   = [8]
    graphData  = zeroCostGraphData

    referenceFn :: Int -> IndexData () ()
    referenceFn =
      \case
        0 -> IndexData () [3]   []
        1 -> IndexData () [5]   []
        2 -> IndexData () [5]   []
        3 -> IndexData () [6,7] [(0, ())]
        4 -> IndexData () [6]   []
        5 -> IndexData () [7]   [(1, ()), (2, ())]
        6 -> IndexData () [8]   [(3, ()), (4, ())]
        7 -> IndexData () [8]   [(3, ()), (5, ())]
        8 -> IndexData () []    [(6, ()), (7, ())]
        _ -> error "Access value outside of exampleNetwork"


candidateNetworkEdgesCases :: TestTree
candidateNetworkEdgesCases = testGroup "Cases of candidateNetworkEdges function"
    [ HU.testCase
        (  "candidateNetworkEdges of tree:\n"
        <> renderExampleTree
        <> "              equals:\n"
        <> renderExampleTreeCandidateNetworkEdges
        )
        candidateNetworkEdgesTreeCase
    , HU.testCase
        (  "candidateNetworkEdges of network:\n"
        <> renderExampleNetwork
        <> "              equals:\n"
        <> renderExampleNetworkCandidateNetworkEdges
        )
        candidateNetworkEdgesNetworkCase

    ]
  where
    candidateNetworkEdgesTreeCase :: Assertion
    candidateNetworkEdgesTreeCase =
            candidateNetworkEdges exampleTree
        @?= exampleTreeCandidateNetworkEdges

    candidateNetworkEdgesNetworkCase :: Assertion
    candidateNetworkEdgesNetworkCase =
            candidateNetworkEdges exampleNetwork
        @?= exampleNetworkCandidateNetworkEdges