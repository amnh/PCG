-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Test.Network
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG.Test.Network
  ( candidateNetworkProperties
  )  where

import Bio.Graph.ReferenceDAG.Utility
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import           Bio.Graph.ReferenceDAG.Internal
import qualified Data.Set as S (toList, fromList)
import Test.QuickCheck (forAll, Property, (===))



candidateNetworkProperties :: TestTree
candidateNetworkProperties = testGroup "Properties of candidateNetworkEdges function"
    [ QC.testProperty
        (    "candidateNetworkEdges of branched binary tree:\n"
          <>  renderBranchedNetwork
          <> "              equals:\n"
          <> "TODO"
        )
        branchedNetworkProperty
    ]
  where
    branchedNetworkProperty :: Property
    branchedNetworkProperty = forAll generateBinaryTree' correctBranchedCandidateEdges
    
    correctBranchedCandidateEdges
      :: ( ReferenceDAG () () ()
         , NetworkInformation
         , NetworkInformation
         )
      -> Property
    correctBranchedCandidateEdges (branchedNet, n0NetInfo, n1NetInfo) =
        candidateNetworkEdges branchedNet === newNetworkEdges
    
      where
        n0NonNetworkEdges = getNonNetworkEdges n0NetInfo
        n1NonNetworkEdges = getNonNetworkEdges n1NetInfo

        newNetworkEdges =
             _candidateNetworkEdges n0NetInfo
          <> _candidateNetworkEdges n1NetInfo
          <> n0n1NetworkEdges

        n0n1NetworkEdges  =
          S.fromList 
            [(n0Edge, n1Edge)
            | n0Edge <- S.toList n0NonNetworkEdges
            , n1Edge <- S.toList n1NonNetworkEdges
            ]
        


renderBranchedNetwork :: String
renderBranchedNetwork = unlines
    ["         r     "
    ,"     ┌───┴───┐ "
    ,"     │       │ "
    ,"     │       │ "
    ,"    n0       n1"
    ]
