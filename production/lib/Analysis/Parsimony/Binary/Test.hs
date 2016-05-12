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
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.PhyloGraph.Solution
import           Data.Alphabet
import           Data.BitMatrix
import           Data.BitVector
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace

standardAlph :: Alphabet String
standardAlph = constructAlphabet $ V.fromList ["A", "C", "G", "T", "-"]

doMeta, fitchMeta :: CharacterMetadata DynamicChar
doMeta    = CharMeta DirectOptimization standardAlph "" False False 1 mempty (emptyChar, emptyChar) 0 (GeneralCost 1 1)
fitchMeta = CharMeta Fitch              standardAlph "" False False 1 mempty (emptyChar, emptyChar) 0 (GeneralCost 1 1)

decodeIt :: DynamicChar -> [[String]]
decodeIt = decodeDynamic standardAlph

testSuite :: TestTree
testSuite = testGroup "Binary optimization" [doProperties, fitchProperties {- , traversalProperties -} ]

-- | Check properties of the DO algorithm
doProperties :: TestTree
doProperties = testGroup "Properties of the DO algorithm"
      [ idHolds
      , firstRow
      , empties
--      , overlap
      ]
    where
        idHolds = testProperty "When DO runs a sequence against itself, get input as result" checkID
            where
                checkID :: DynamicChar -> Bool
                checkID inSeq = trace ("main result of DO " ++ show main ++ " " ++ show cost)
                                main == inSeq && cost == 0 && gapped == inSeq && left == inSeq && right == inSeq
                    where (main, cost, gapped, left, right) = naiveDO inSeq inSeq doMeta

        firstRow = testProperty "First row of alignment matrix has expected directions" checkRow
            where
                checkRow :: DynamicChar -> Bool
                checkRow inSeq = --trace ("checkRow " ++ show result ++ show rowLen) $
                                    (snd $ V.head result) == DiagDir && allLeft (V.tail result) && V.length result == rowLen
                    where
                        rowLen = numChars inSeq
                        (result, seqs) = firstAlignRow inSeq rowLen 0 0 doMeta
                        allLeft = V.all (\val -> snd val == LeftDir)

        empties = testProperty "NaiveDO correctly handles an empty sequence" checkEmpty
            where
                checkEmpty :: DynamicChar -> Bool
                checkEmpty inSeq = main == inSeq && cost == 0
                    where (main, cost, gapped, left, right) = naiveDO inSeq emptyChar doMeta

        {-
        overlap = testGroup "Overlap test cases" [overlap1]
        seqa = encodeOverAlphabet standardAlph $ V.fromList [["G", "C"]]
        seqb =  encodeOverAlphabet standardAlph $ V.fromList [["C"]]
        andOverlap = decodeIt $ fromRows $ pure $ fst $ getOverlap (grabSubChar seqa 0) (grabSubChar seqb 0) doMeta
        andOverlapResult = V.fromList [["C"]]
        overlap1 = testCase "Given characters with overlap, gives zero cost" (andOverlapResult @=? andOverlap)
        -}

-- | Check properties of the Fitch algorithm
fitchProperties :: TestTree
fitchProperties = testGroup "Properties of the Fitch algorithm" [preIdHolds, postIdHolds]
    where
        preIdHolds = testProperty "When Preorder Fitch runs a sequence against itself, get input as result" checkID
            where
                checkID :: DynamicChar -> Bool
                checkID inSeq = result == inSeq && cost == 0
                    where (result, _, cost) = preorderFitchBit 1 inSeq inSeq fitchMeta

        postIdHolds = testProperty "When Postorder Fitch runs a sequence against itself, get input as result" checkID
            where
                checkID :: DynamicChar -> Bool
                checkID inSeq = result == inSeq
                    where result = postorderFitchBit inSeq inSeq inSeq inSeq inSeq fitchMeta


-- | Check properties of the traversal
--traversalProperties :: TestTree
--traversalProperties = undefined
