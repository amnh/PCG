-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests
  ( candidateNetworkProperties
  )  where

import           Bio.Graph.ReferenceDAG.Internal hiding (fromList)
import           Bio.Graph.ReferenceDAG.Network
import           Bio.Graph.ReferenceDAG.Utility
import           Control.Applicative             ((<|>))
import           Control.Lens.Operators          ((^.))
import           Data.Foldable
import qualified Data.List.NonEmpty              as NE
import           Data.Set                        (Set, cartesianProduct, fromList, intersection)
import           Test.QuickCheck                 (Property, forAll, (===))
import           Test.Tasty
import qualified Test.Tasty.QuickCheck           as QC



candidateNetworkProperties :: TestTree
candidateNetworkProperties = testGroup "Properties of candidateNetworkEdges function"
    [ QC.testProperty
        ( unlines
          [ "Given valid networks n0 and n1, the candidateNetworkEdges of a branched binary"
          , "      network of the following form:"
          ,  renderBranchedNetwork
          , "      is equal to the candidate network edges of n0 plus the candidate network"
          , "      edges of n1 plus those edges from a non-network adjacent edge in n0 or n1 to"
          , "      a non-network edge in n1 or n0 (respectively)."
          ]
        )
        branchedNetworkProperty
    , QC.testProperty
        ( unlines
          [ "Given valid networks n0, n1 and n2 (with roots r0, r1 and r2), the "
          , "      candidateNetworkEdges of a doubly branched binary network:"
          ,  renderDoublyBranchedNetwork
          , "      should not contain edges from (r,x), (x,r1) to any edge in n0 or n1."
          ]
        )
        doublyBranchedNetworkProperty
    , QC.testProperty
        ( unlines $
          [ "Given valid networks n0, n1 and n2 (with roots r0, r1 and r2), the "
          , "      candidateNetworkEdges of a network with the following shape: "
          ,  renderDoublyBranchedNetworkWithNetworkEvent
          , "      should not contain edges from (r,a), (r, r0), (a,r1), (x, r2)"
          , "      to any edge in n0 or n1."
          ]
        )
        branchedNetworkWithNetworkEventProperty
    ]
  where
    branchedNetworkProperty :: Property
    branchedNetworkProperty = forAll generateBranchedNetwork correctBranchedCandidateEdges

    correctBranchedCandidateEdges
      :: ( ReferenceDAG () () ()
         , NetworkInformation
         , NetworkInformation
         )
      -> Property
    correctBranchedCandidateEdges (branchedNet, n0NetInfo, n1NetInfo) =
          lhs === rhs

      where
        lhs = candidateNetworkEdges branchedNet
        rhs = newNetworkEdges

        n0NonNetworkEdges = getNonNetworkEdges n0NetInfo
        n1NonNetworkEdges = getNonNetworkEdges n1NetInfo
        n0Edges = _edgeSet n0NetInfo
        n1Edges = _edgeSet n1NetInfo

        newNetworkEdges = fold
          [ _candidateNetworkEdges n0NetInfo
          , _candidateNetworkEdges n1NetInfo
          , cartesianProduct n0Edges n1NonNetworkEdges
          , cartesianProduct n1Edges n0NonNetworkEdges
          ]

    doublyBranchedNetworkProperty :: Property
    doublyBranchedNetworkProperty =
        forAll generateDoublyBranchedNetwork noAncestorEdges
      where
        noAncestorEdges
          :: ( ReferenceDAG () () ()
             , NetworkInformation
             , NetworkInformation
             , NetworkInformation
             , Int
             )
          -> Property
        noAncestorEdges (dag, _, n1NetInfo, n2NetInfo, n0n1RootIndex)
            = disallowedEdges `intersection` candEdges === mempty
          where

            candEdges = candidateNetworkEdges dag
            rootN1  = _rootNode n1NetInfo
            rootN2  = _rootNode n2NetInfo
            root    = NE.head $ dag ^. _rootRefs
            rxEdge  = (root, n0n1RootIndex)
            xN1Edge = (n0n1RootIndex, rootN1)
            xN2Edge = (n0n1RootIndex, rootN2)
            n1Edges = toList $ _edgeSet n1NetInfo
            n2Edges = toList $ _edgeSet n2NetInfo

            disallowedEdges :: Set ((Int, Int), (Int, Int))
            disallowedEdges = fromList $
              do
                ancestralEdge <- [rxEdge, xN1Edge, xN2Edge]
                n1n2Edge      <- n1Edges <|> n2Edges
                pure (ancestralEdge, n1n2Edge)




    branchedNetworkWithNetworkEventProperty :: Property
    branchedNetworkWithNetworkEventProperty =
        forAll generateBranchedNetworkWithNetworkEvent ancestralNetworkEventTest
      where
        ancestralNetworkEventTest
          :: ( ReferenceDAG () () ()
             , NetworkInformation
             , NetworkInformation
             , NetworkInformation
             , Int
             , Int
             , Int
             )
          -> Property
        ancestralNetworkEventTest
            ( dag
            , n0NetInfo
            , n1NetInfo
            , n2NetInfo
            , aIndex
            , bIndex
            , cIndex
            ) = disallowedEdges `intersection` candEdges === mempty

          where

            candEdges = candidateNetworkEdges dag
            abEdge    = (aIndex, bIndex)
            acEdge    = (aIndex, cIndex)
            n0Edges   = toList $ _edgeSet n0NetInfo
            n1Edges   = toList $ _edgeSet n1NetInfo
            n2Edges   = toList $ _edgeSet n2NetInfo

            disallowedEdges = fromList $
              do
                ancestralEdge <- [abEdge, acEdge]
                n0n1n2Edge    <- n0Edges <|> n1Edges <|> n2Edges
                pure (ancestralEdge, n0n1n2Edge)











renderBranchedNetwork :: String
renderBranchedNetwork = unlines
    ["                                 r     "
    ,"                             ┌───┴───┐ "
    ,"                             │       │ "
    ,"                             │       │ "
    ,"                            n0       n1"
    ]



renderDoublyBranchedNetwork :: String
renderDoublyBranchedNetwork = unlines
    ["                                 r         "
    ,"                             ┌───┴───┐     "
    ,"                             │       │     "
    ,"                             │       │     "
    ,"                            n2       x     "
    ,"                                 ┌───┴───┐ "
    ,"                                 │       │ "
    ,"                                 │       │ "
    ,"                                n0       n1"
    ]




renderDoublyBranchedNetworkWithNetworkEvent :: String
renderDoublyBranchedNetworkWithNetworkEvent = unlines
    [ "                    r              "
    , "                ┌───┴───┐          "
    , "                │       │          "
    , "                │       │          "
    , "                n3      a          "
    , "                   ┌────┴────┐     "
    , "                   │         │     "
    , "                   │         │     "
    , "                   b         c     "
    , "               ┌───┴───┐ ┌───┴───┐ "
    , "               │       │ │       │ "
    , "               │       │ │       │ "
    , "               │       └┬┘       │ "
    , "              n0        n1       n2"
    ]
