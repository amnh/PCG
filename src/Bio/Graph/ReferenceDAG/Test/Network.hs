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
import Data.Set (Set)
import Test.QuickCheck (forAll, Property)



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
    branchedNetworkProperty = forAll generateBranchedNetwork correctBranchedCandidateEdges
    
    correctBranchedCandidateEdges
      :: ( ReferenceDAG () () ()
         , Set ((Int, Int), (Int, Int))
         , Set ((Int, Int), (Int, Int))
         )
      -> Bool
    correctBranchedCandidateEdges (branchedNet, n0CandEdges, n1CandEdges) =
      undefined


renderBranchedNetwork :: String
renderBranchedNetwork = unlines
    ["         r     "
    ,"     ┌───┴───┐ "
    ,"     │       │ "
    ,"     │       │ "
    ,"    n0       n1"
    ]
