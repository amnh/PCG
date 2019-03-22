{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Test.NetworkUnitTests
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG.Test.NetworkUnitTests
  ( candidateNetworkEdgesCases
  )  where

import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Graph.ReferenceDAG.Network
import           Data.Foldable                   (fold)
import           Data.Set                        (Set)
import qualified Data.Vector                     as V
import           Test.Tasty
import           Test.Tasty.HUnit                as HU


candidateNetworkEdgesCases :: TestTree
candidateNetworkEdgesCases = testGroup "Cases of candidateNetworkEdges function"
    [ HU.testCase
        (fold @[] @String
           [ "candidateNetworkEdges of tree:\n"
           ,  renderExampleTree
           ,  "              equals:\n"
           , renderExampleTreeCandidateNetworkEdges
           ])
        candidateNetworkEdgesTreeCase
    , HU.testCase
        (fold @[] @String
           [ "candidateNetworkEdges of network:\n"
           ,  renderExampleNetwork
           , "              equals:\n"
           ,  renderExampleNetworkCandidateNetworkEdges
           ])
        candidateNetworkEdgesNetworkCase
    , HU.testCase
        (fold @[] @String
            [ "candidateNetworkEdges of tree:\n"
            ,  renderExampleTree2
            , "              equals:\n"
            ,  renderExampleTree2CandidateNetworkEdges
            ])
        candidateNetworkEdgesTree2Case
     , HU.testCase
        (fold @[] @String
           [ "candidateNetworkEdges of network:\n"
           ,  renderExampleNetwork2
           , "              equals:\n"
           ,  renderExampleNetwork2CandidateNetworkEdges
           ])
        candidateNetworkEdgesNetwork2Case

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

    candidateNetworkEdgesTree2Case :: Assertion
    candidateNetworkEdgesTree2Case =
            candidateNetworkEdges exampleTree2
        @?= exampleTree2CandidateNetworkEdges

    candidateNetworkEdgesNetwork2Case :: Assertion
    candidateNetworkEdgesNetwork2Case =
            candidateNetworkEdges exampleNetwork2
        @?= exampleNetwork2CandidateNetworkEdges


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
    , "                {"
    , "                       ((4,0), (5,2)), ((4,0), (5,3)), ((4,0), (6,5))"
    , "                       ((4,1), (5,2)), ((4,1), (5,3)), ((4,1), (6,5))"
    , "                       ((5,2), (4,0)), ((5,2), (4,1)), ((5,2), (6,4))"
    , "                       ((5,3), (4,0)), ((5,3), (4,1)), ((5,3), (6,4))"
    , "                       ((6,4), (5,2)), ((6,4), (5,3))"
    , "                       ((6,5), (4,0)), ((6,5), (4,1))"
    , "                }"
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
        _ -> error "Access value outside of network"



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
    [ ((3, 0), (5, 1)), ((3, 0), (5, 2))
    , ((6, 4), (5, 1)), ((6, 4), (5, 2))
    , ((7, 3), (5, 1)), ((7, 3), (5, 2))
    ]


renderExampleNetworkCandidateNetworkEdges :: String
renderExampleNetworkCandidateNetworkEdges
    = unlines
    [ "             Candidate Edge Pairs: "
    , "               {"
    , "                    ((3, 0), (5, 1)), ((3, 0), (5, 2))"
    , "                    ((6, 4), (5, 1)), ((6, 4), (5, 2))"
    , "               }"
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
        _ -> error "Access value outside of network"




renderExampleTree2 :: String
renderExampleTree2 = unlines
                      [ "                       10"
                      , "                     /   \\"
                      , "                    6     9"
                      , "                   /  \\  /  \\"
                      , "                  /   5  7   8"
                      , "                 2   / \\"
                      , "               / |  3   4"
                      , "              0  1"

                      ]

exampleTree2CandidateNetworkEdges :: Set ((Int, Int), (Int, Int))
exampleTree2CandidateNetworkEdges =
    [ ((2,0),(5,3)), ((2,0),(5,4))
    , ((2,0),(9,7)), ((2,0),(9,8))
    , ((2,1),(5,3)), ((2,1),(5,4))
    , ((2,1),(9,7)), ((2,1),(9,8))
    , ((5,3),(2,0)), ((5,3),(2,1))
    , ((5,3),(9,7)), ((5,3),(9,8))
    , ((5,4),(2,0)), ((5,4),(2,1))
    , ((5,4),(9,7)), ((5,4),(9,8))
    , ((6,2),(5,3)), ((6,2),(5,4))
    , ((6,2),(9,7)), ((6,2),(9,8))
    , ((6,5),(2,0)), ((6,5),(2,1))
    , ((6,5),(9,7)), ((6,5),(9,8))
    , ((9,7),(2,0)), ((9,7),(2,1))
    , ((9,7),(5,3)), ((9,7),(5,4))
    , ((9,7),(6,2)), ((9,7),(6,5))
    , ((9,8),(2,0)), ((9,8),(2,1))
    , ((9,8),(5,3)), ((9,8),(5,4))
    , ((9,8),(6,2)), ((9,8),(6,5))
    ]



renderExampleTree2CandidateNetworkEdges :: String
renderExampleTree2CandidateNetworkEdges
    = unlines
    [ "              Candidate Edge Pairs: "
    , "                {"
    , "                      ((2,0),(5,3)), ((2,0),(5,4))"
    , "                      ((2,0),(9,7)), ((2,0),(9,8))"
    , "                      ((2,1),(5,3)), ((2,1),(5,4))"
    , "                      ((2,1),(9,7)), ((2,1),(9,8))"
    , "                      ((5,3),(2,0)), ((5,3),(2,1))"
    , "                      ((5,3),(9,7)), ((5,3),(9,8))"
    , "                      ((5,4),(2,0)), ((5,4),(2,1))"
    , "                      ((5,4),(9,7)), ((5,4),(9,8))"
    , "                      ((6,2),(5,3)), ((6,2),(5,4))"
    , "                      ((6,2),(9,7)), ((6,2),(9,8))"
    , "                      ((6,5),(2,0)), ((6,5),(2,1))"
    , "                      ((6,5),(9,7)), ((6,5),(9,8))"
    , "                      ((9,7),(2,0)), ((9,7),(2,1))"
    , "                      ((9,7),(5,3)), ((9,7),(5,4))"
    , "                      ((9,7),(6,2)), ((9,7),(6,5))"
    , "                      ((9,8),(2,0)), ((9,8),(2,1))"
    , "                      ((9,8),(5,3)), ((9,8),(5,4))"
    , "                      ((9,8),(6,2)), ((9,8),(6,5))"
    , "                }"
    ]

exampleTree2 :: ReferenceDAG () () ()
exampleTree2 = ReferenceDAG{..}
  where
    references = V.generate 11 referenceFn
    rootRefs   = [10]
    graphData  = zeroCostGraphData

    referenceFn :: Int -> IndexData () ()
    referenceFn =
      \case
        0 -> IndexData ()  [2]  []
        1 -> IndexData ()  [2]  []
        2 -> IndexData ()  [6]  [(0, ()), (1, ())]
        3 -> IndexData ()  [5]  []
        4 -> IndexData ()  [5]  []
        5 -> IndexData ()  [6]  [(3, ()), (4, ())]
        6 -> IndexData ()  [10] [(2, ()), (5, ())]
        7 -> IndexData ()  [9]  []
        8 -> IndexData ()  [9]  []
        9 -> IndexData ()  [10] [(7, ()), (8, ())]
        10 -> IndexData () []   [(6, ()), (9, ())]
        _ -> error "Access value outside of network"


renderExampleNetwork2 :: String
renderExampleNetwork2 = unlines
                         [ "                        18"
                         , "                      /     \\"
                         , "                    16      17"
                         , "                   /  \\     /  \\"
                         , "                  7    \\   /   15"
                         , "                        14     /  \\"
                         , "                         |   13    \\"
                         , "                        12   / \\    \\"
                         , "                        / \\  5  6    \\"
                         , "                       3   4         11"
                         , "                                     / \\"
                         , "                                    9  10"
                         , "                                   / \\/ \\"
                         , "                                  1   8   2"
                         , "                                      |"
                         , "                                      0"
                         ]


exampleNetwork2CandidateNetworkEdges :: Set ((Int, Int), (Int, Int))
exampleNetwork2CandidateNetworkEdges
  = [ ((8,0),(12,3))  , ((8,0),(12,4))  , ((8,0),(13,5))   , ((8,0),(13,6))
    , ((9,1),(12,3))  , ((9,1),(12,4))  , ((9,1),(13,5))   , ((9,1),(13,6))
    , ((9,8),(12,3))  , ((9,8),(12,4))  , ((9,8),(13,5))   , ((9,8),(13,6))
    , ((10,2),(12,3)) , ((10,2),(12,4)) , ((10,2),(13,5))  , ((10,2),(13,6))
    , ((10,8),(12,3)) , ((10,8),(12,4)) , ((10,8),(13,5))  , ((10,8),(13,6))
    , ((11,9),(12,3)) , ((11,9),(12,4)) , ((11,9),(13,5))  , ((11,9),(13,6))
    , ((11,10),(12,3)), ((11,10),(12,4)), ((11,10),(13,5)) , ((11,10),(13,6))
    , ((12,3),(9,1))  , ((12,3),(10,2)) , ((12,3),(11,9))  , ((12,3),(11,10))
    , ((12,3),(13,5)) , ((12,3),(13,6)) , ((12,3),(15,11)) , ((12,3),(15,13))
    , ((12,4),(9,1))  , ((12,4),(10,2)) , ((12,4),(11,9))  , ((12,4),(11,10))
    , ((12,4),(13,5)) , ((12,4),(13,6)) , ((12,4),(15,11)) , ((12,4),(15,13))
    , ((13,5),(9,1))  , ((13,5),(10,2)) , ((13,5),(11,9))  , ((13,5),(11,10))
    , ((13,5),(12,3)) , ((13,5),(12,4)) , ((13,6),(9,1))   , ((13,6),(10,2))
    , ((13,6),(11,9)) , ((13,6),(11,10)), ((13,6),(12,3))  , ((13,6),(12,4))
    , ((14,12),(9,1)) , ((14,12),(10,2)), ((14,12),(11,9)) , ((14,12),(11,10))
    , ((14,12),(13,5)), ((14,12),(13,6)), ((14,12),(15,11)), ((14,12),(15,13))
    , ((15,11),(12,3)), ((15,11),(12,4)), ((15,11),(13,5)) , ((15,11),(13,6))
    , ((15,13),(9,1)) , ((15,13),(10,2)), ((15,13),(11,9)) , ((15,13),(11,10))
    , ((15,13),(12,3)), ((15,13),(12,4)), ((16,7),(9,1))   , ((16,7),(10,2))
    , ((16,7),(11,9)) , ((16,7),(11,10)), ((16,7),(12,3))  , ((16,7),(12,4))
    , ((16,7),(13,5)) , ((16,7),(13,6)) , ((16,7),(15,11)) , ((16,7),(15,13))
    , ((17,14),(9,1)) , ((17,14),(10,2)), ((17,14),(11,9)) , ((17,14),(11,10))
    , ((17,14),(13,5)), ((17,14),(13,6)), ((17,14),(15,11)), ((17,14),(15,13))
    , ((17,15),(12,3)), ((17,15),(12,4))
    ]

renderExampleNetwork2CandidateNetworkEdges :: String
renderExampleNetwork2CandidateNetworkEdges
    = unlines
    [ "Candidate Edge Pairs: "
    , "    {"
    , "        [ ((8,0),(12,3))  , ((8,0),(12,4))  , ((8,0),(13,5))   , ((8,0),(13,6))   "
    , "        , ((9,1),(12,3))  , ((9,1),(12,4))  , ((9,1),(13,5))   , ((9,1),(13,6))   "
    , "        , ((9,8),(12,3))  , ((9,8),(12,4))  , ((9,8),(13,5))   , ((9,8),(13,6))   "
    , "        , ((10,2),(12,3)) , ((10,2),(12,4)) , ((10,2),(13,5))  , ((10,2),(13,6))  "
    , "        , ((10,8),(12,3)) , ((10,8),(12,4)) , ((10,8),(13,5))  , ((10,8),(13,6))  "
    , "        , ((11,9),(12,3)) , ((11,9),(12,4)) , ((11,9),(13,5))  , ((11,9),(13,6))  "
    , "        , ((11,10),(12,3)), ((11,10),(12,4)), ((11,10),(13,5)) , ((11,10),(13,6)) "
    , "        , ((12,3),(9,1))  , ((12,3),(10,2)) , ((12,3),(11,9))  , ((12,3),(11,10)) "
    , "        , ((12,3),(13,5)) , ((12,3),(13,6)) , ((12,3),(15,11)) , ((12,3),(15,13)) "
    , "        , ((12,4),(9,1))  , ((12,4),(10,2)) , ((12,4),(11,9))  , ((12,4),(11,10)) "
    , "        , ((12,4),(13,5)) , ((12,4),(13,6)) , ((12,4),(15,11)) , ((12,4),(15,13)) "
    , "        , ((13,5),(9,1))  , ((13,5),(10,2)) , ((13,5),(11,9))  , ((13,5),(11,10)) "
    , "        , ((13,5),(12,3)) , ((13,5),(12,4)) , ((13,6),(9,1))   , ((13,6),(10,2))  "
    , "        , ((13,6),(11,9)) , ((13,6),(11,10)), ((13,6),(12,3))  , ((13,6),(12,4))  "
    , "        , ((14,12),(9,1)) , ((14,12),(10,2)), ((14,12),(11,9)) , ((14,12),(11,10))"
    , "        , ((14,12),(13,5)), ((14,12),(13,6)), ((14,12),(15,11)), ((14,12),(15,13))"
    , "        , ((15,11),(12,3)), ((15,11),(12,4)), ((15,11),(13,5)) , ((15,11),(13,6)) "
    , "        , ((15,13),(9,1)) , ((15,13),(10,2)), ((15,13),(11,9)) , ((15,13),(11,10))"
    , "        , ((15,13),(12,3)), ((15,13),(12,4)), ((16,7),(9,1))   , ((16,7),(10,2))  "
    , "        , ((16,7),(11,9)) , ((16,7),(11,10)), ((16,7),(12,3))  , ((16,7),(12,4))  "
    , "        , ((16,7),(13,5)) , ((16,7),(13,6)) , ((16,7),(15,11)) , ((16,7),(15,13)) "
    , "        , ((17,14),(9,1)) , ((17,14),(10,2)), ((17,14),(11,9)) , ((17,14),(11,10))"
    , "        , ((17,14),(13,5)), ((17,14),(13,6)), ((17,14),(15,11)), ((17,14),(15,13))"
    , "        , ((17,15),(12,3)), ((17,15),(12,4))                                      "
    , "    }"
    ]

exampleNetwork2 :: ReferenceDAG () () ()
exampleNetwork2 = ReferenceDAG{..}
  where
    references = V.generate 19 referenceFn
    rootRefs   = [18]
    graphData  = zeroCostGraphData

    referenceFn :: Int -> IndexData () ()
    referenceFn =
      \case
        0  -> IndexData () [8]      []
        1  -> IndexData () [9]      []
        2  -> IndexData () [10]     []
        3  -> IndexData () [12]     []
        4  -> IndexData () [12]     []
        5  -> IndexData () [13]     []
        6  -> IndexData () [13]     []
        7  -> IndexData () [16]     []
        8  -> IndexData () [9, 10]  [(0 ,())]
        9  -> IndexData () [11]     [(1 ,()), (8 ,())]
        10 -> IndexData () [11]     [(2 ,()), (8 ,())]
        11 -> IndexData () [15]     [(9,()) , (10,())]
        12 -> IndexData () [14]     [(3 ,()), (4 ,())]
        13 -> IndexData () [15]     [(5 ,()), (6 ,())]
        14 -> IndexData () [16, 17] [(12,())]
        15 -> IndexData () [17]     [(11,()), (13,())]
        16 -> IndexData () [18]     [(7 ,()), (14,())]
        17 -> IndexData () [18]     [(14,()), (15,())]
        18 -> IndexData () []       [(16,()), (17,())]
        _  -> error "Access value outside of network"
