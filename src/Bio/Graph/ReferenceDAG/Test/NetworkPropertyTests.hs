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

import Bio.Graph.ReferenceDAG.Utility
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import           Bio.Graph.ReferenceDAG.Internal hiding (fromList)
import qualified Data.Set as S (toList, fromList, difference)
import Test.QuickCheck (forAll, Property, (===))
import Control.Applicative ((<|>))
import Data.Set (notMember, singleton, disjoint, fromList, Set, toList)
import qualified Data.List.NonEmpty as NE
import Control.Lens.Operators ((^.))
import Control.Applicative ((<|>))

import Debug.Trace



candidateNetworkProperties :: TestTree
candidateNetworkProperties = testGroup "Properties of candidateNetworkEdges function"
    [ QC.testProperty
        ( unlines $
          [ "Given valid networks n0 and n1, the candidateNetworkEdges of a branched binary"
          , "      network:"
          ,  renderBranchedNetwork
          , "      are equal to the candidate network edges of n0 plus the candidate network"
          , "      edges of n1 plus those edges from a non-network adjacent edge in n0 or n1 to"
          , "      a non-network edge in n1 or n0 (respectively)."
          ]
        )
        branchedNetworkProperty
    , QC.testProperty
        ( unlines $
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
          , "      should not contain edges from (r,x), (r, r0), (x,r1), (x, r2)"
          , "      to any edge in n0 or n1."
          ]
        )
        doublyBranchedNetworkProperty
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

        newNetworkEdges =
              _candidateNetworkEdges n0NetInfo
           <> _candidateNetworkEdges n1NetInfo
           <> n0n1NetworkEdges

        n0n1NetworkEdges  =
          S.fromList $ do
            n0Edge <- S.toList  n0NonNetworkEdges
            n1Edge <- S.toList n1NonNetworkEdges
            pure (n0Edge, n1Edge) <|> pure (n1Edge, n0Edge)
 

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
          -> Bool
        noAncestorEdges (dag, _, n1NetInfo, n2NetInfo, n0n1RootIndex)
            = disallowedEdges `disjoint` candEdges
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
             , NetworkInformation
             , Int
             , Int
             , Int
             )
          -> Bool
        ancestralNetworkEventTest
            ( dag
            , n0NetInfo
            , n1NetInfo
            , n2NetInfo
            , n3NetInfo
            , aIndex
            , bIndex
            , cIndex
            ) = disallowedEdges `disjoint` candEdges
              
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
