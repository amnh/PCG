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
--    , QC.testProperty
--        ( unlines $
--          [ "Given valid networks n0, n1 and n2 (with roots r0, r1 and r2), the "
--            "      candidateNetworkEdges of a doubly branched binary network:"
--          ,  renderDoublyBranchedNetwork
--          , "      should not contain edges from (r,x), (x,r1) to any edge in n0 or n1."
--          ]
--        )
--        doublyBranchedNetworkProperty
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
        candidateNetworkEdges branchedNet === newNetworkEdges
    
      where
        n0NonNetworkEdges = getNonNetworkEdges n0NetInfo
        n1NonNetworkEdges = getNonNetworkEdges n1NetInfo

        newNetworkEdges =
              _candidateNetworkEdges n0NetInfo
           <> _candidateNetworkEdges n1NetInfo
           <> n0n1NetworkEdges

        n0n1NetworkEdges  =
          S.fromList $ do
            n0Edge <- S.toList n0NonNetworkEdges
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
             )
          -> Property
        noAncestorEdges arg = error "TODO"
         
         
         


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
