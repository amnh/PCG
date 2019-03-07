{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Graph.ReferenceDAG.Test
  ( testSuite
  ) where

import           Bio.Graph.ReferenceDAG.Internal
import qualified Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests as NPT
import           Bio.Graph.ReferenceDAG.Test.NetworkUnitTests     as NUT
import           Data.Set                        as Set
import qualified Data.Vector                     as V
import           Test.Tasty
import           Test.Tasty.HUnit                as HU


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
