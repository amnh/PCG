------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG.Test
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG.Test
  ( testSuite
  ) where

import qualified Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests as NPT
import           Bio.Graph.ReferenceDAG.Test.NetworkUnitTests     as NUT
import           Test.Tasty


-- |
-- Test-suite including specific unit and property-based tests for the
-- 'Bio.Graph.ReferenceDAG.ReferenceDAG' data-type.
testSuite :: TestTree
testSuite = testGroup "ReferenceDAG Tests"
    [ testExampleCases
    , testPropertyCases
    ]

testExampleCases :: TestTree
testExampleCases = testGroup "Example Cases for Bio.Graph.ReferenceDAG"
    [ NUT.candidateNetworkEdgesCases
    ]

testPropertyCases :: TestTree
testPropertyCases = testGroup "Property tests for Bio.Graph.ReferenceDAG"
    [ NPT.candidateNetworkProperties
    ]
