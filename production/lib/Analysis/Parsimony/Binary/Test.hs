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

import           Analysis.Parsimony.Binary.DirectOptimization
import           Analysis.Parsimony.Binary.Fitch
import           Analysis.Parsimony.Binary.Internal
import           Bio.Metadata
import           Bio.Sequence.Coded
import           Bio.PhyloGraph.Solution
import           Data.BitVector
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

doMeta, fitchMeta :: CharacterMetadata EncodedSeq
doMeta = CharMeta DirectOptimization ["A", "C", "G", "T", "-"] "" False False 1 mempty mempty 0 (GeneralCost 1 1)
fitchMeta = CharMeta Fitch ["A", "C", "G", "T", "-"] "" False False 1 mempty mempty 0 (GeneralCost 1 1)

testSuite :: TestTree
testSuite = testGroup "Binary optimization" [doProperties, fitchProperties {- , traversalProperties -} ]

-- | Check properties of the DO algorithm
doProperties :: TestTree
doProperties = testGroup "Properties of the DO algorithm" [idHolds, firstRow, empties]
    where
        idHolds = testProperty "When DO runs a sequence against itself, get input as result" checkID
            where
                checkID :: EncodedSeq -> Bool
                checkID inSeq = main == inSeq && cost == 0 && gapped == inSeq && left == inSeq && right == inSeq
                    where (main, cost, gapped, left, right) = naiveDO inSeq inSeq doMeta

        firstRow = testProperty "First row of alignment matrix has expected directions" checkRow
            where
                checkRow :: EncodedSeq -> Bool
                checkRow inSeq = (snd $ V.head result) == DiagDir && allLeft (V.tail result) && V.length result == rowLen
                    where
                        rowLen = numChars inSeq 5
                        (result, seqs) = firstAlignRow inSeq rowLen 0 0 doMeta
                        allLeft = V.all (\val -> snd val == LeftDir)

        empties = testProperty "NaiveDO correctly handles an empty sequence" checkEmpty
            where
                checkEmpty :: EncodedSeq -> Bool
                checkEmpty inSeq = main == inSeq && cost == 0
                    where (main, cost, gapped, left, right) = naiveDO inSeq mempty doMeta

-- | Check properties of the Fitch algorithm
fitchProperties :: TestTree
fitchProperties = testGroup "Properties of the Fitch algorithm" [idHolds]
    where
        idHolds = testProperty "When Fitch runs a sequence against itself, get input as result" checkID
            where
                checkID :: EncodedSeq -> Bool
                checkID inSeq = result == inSeq && cost == 0
                    where (result, _, cost) = preorderFitchBit 1 inSeq inSeq fitchMeta

-- | Check properties of the traversal
--traversalProperties :: TestTree
--traversalProperties = undefined
