-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for binary optimization
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Binary.Test where

import Analysis.Parsimony.Binary.DirectOptimization
import Analysis.Parsimony.Binary.Internal

import Bio.Sequence.Coded
import Bio.PhyloGraph.Graph
import Data.BitVector
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testSuite :: TestTree
testSuite = testGroup "Binary optimization" [doProperties]

-- | Check properties of the DO algorithm
doProperties :: TestTree
doProperties = undefined --testGroup "Properties of the DO algorithm" [idHolds]
    --where
    --    idHolds = testProperty "When DO runs a sequence against itself, get input as result" checkID
    --        where
    --            checkID :: EncodedSeq BitVector -> Bool
    --            checkID inSeq = main == inSeq && cost == 0 && gapped == inSeq && left == inSeq && right == inSeq
    --                where (main, cost, gapped, left, right) = naiveDO inSeq inSeq
