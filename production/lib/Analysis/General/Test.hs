-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.General.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for general analysis operations
--
-----------------------------------------------------------------------------

module Analysis.General.Test where

import           Analysis.General.NeedlemanWunsch
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.Metadata
import           Data.Alphabet
import           Data.BitMatrix
import           Data.Matrix.NotStupid (getRow)
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
   
standardAlph :: Alphabet String
standardAlph = constructAlphabet $ V.fromList ["A", "C", "G", "T", "-"]

sampleMeta :: CharacterMetadata DynamicChar             
sampleMeta    = CharMeta DirectOptimization standardAlph "" False False 1 mempty (emptyChar, emptyChar) 0 (GeneralCost 1 1)

testSuite :: TestTree
testSuite = testGroup "General analysis functionality" [needlemanProperties]

needlemanProperties :: TestTree
needlemanProperties = testGroup "Properties of Needleman Wunsch algorithm" [firstRow, overlap]
    where
        firstRow = testProperty "First row of alignment matrix has expected directions" checkRow
            where
                checkRow :: DynamicChar -> Bool
                checkRow inSeq = --trace ("checkRow " ++ show result ++ show rowLen) $
                                    fDir == DiagDir && allLeft (V.tail result) && V.length result == (rowLen + 1)
                    where
                        rowLen = numChars inSeq
                        fullMat = getAlignMat inSeq inSeq sampleMeta
                        result = getRow 0 fullMat
                        (_, fDir, _) = V.head result
                        allLeft = V.all (\(_, val, _) -> val == LeftDir)

        overlap = testGroup "Overlap test cases" [overlap1]
        seqa = encodeDynamic standardAlph [["G", "C"]] :: DynamicChar
        seqb =  encodeDynamic standardAlph [["C"]] :: DynamicChar
        initalResult = getOverlap (grabSubChar seqa 0) (grabSubChar seqb 0) sampleMeta
        andOverlap = decodeDynamic standardAlph . DC . fromRows . V.singleton $ fst initalResult
        andOverlapResult = [["C"]]
        overlap1 = testCase "Given characters with overlap, gives expected results" ((andOverlapResult, 0) @=? (andOverlap, snd initalResult))