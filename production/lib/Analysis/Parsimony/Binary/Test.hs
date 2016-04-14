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

import           Bio.Metadata
import           Bio.Sequence.Coded
import           Bio.Sequence.Random
import           Bio.PhyloGraph.Solution
import           Data.BitVector
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

doMeta, fitchMeta :: CharacterMetadata EncodedSeq
doMeta = CharMeta DirectOptimization ["A", "C", "G", "T", "-"] "" False False 1 mempty mempty mempty 0
fitchMeta = CharMeta Fitch ["A", "C", "G", "T", "-"] "" False False 1 mempty mempty mempty 0

testSuite :: TestTree
testSuite = testGroup "Binary optimization" [doProperties]

-- | Check properties of the DO algorithm
doProperties :: TestTree
doProperties = testGroup "Properties of the DO algorithm" [idHolds, firstRow, checkEmpty]
    where
        idHolds = testProperty "When DO runs a sequence against itself, get input as result" checkID
            where
                checkID :: EncodedSeq -> Bool
                checkID inSeq = main == inSeq && cost == 0 && gapped == inSeq && left == inSeq && right == inSeq
                    where (main, cost, gapped, left, right) = naiveDO inSeq inSeq doMeta

        firstRow = testProperty "First row of alignment matrix has expected directions" checkRow
            where
                checkRow :: EncodedSeq -> Bool
                checkRow inSeq = (V.head dirs) == DiagDir && allLeft (V.tail dirs) && V.length costs == rowLen
                    where
                        alphLen = 5
                        rowLen = numChars inSeq alphLen
                        (costs, seqs, dirs) = firstAlignRow 1 inSeq rowLen 0 0 alphLen
                        allLeft = V.all ((==) LeftDir)

        empties = testProperty "NaiveDO correctly handles an empty sequence"
            where
                checkEmpty :: EncodedSeq -> Bool
                checkEmpty inSeq = main == inSeq && cost == 0
                    where (main, cost, gapped, left, right) = naiveDO inSeq mempty doMeta

-- | Check properties of the Fitch algorithm
fitchProperties :: TestTree
fitchProperties = undefined

-- | Check properties of the traversal
traversalProperties :: TestTree
traversalProperties = undefined
