-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.DirectOptimization.Test
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

module Analysis.Parsimony.Binary.DirectOptimization.Test where

import           Analysis.Parsimony.Binary.DirectOptimization.Internal
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.Metadata
import           Bio.Metadata.Internal
import           Data.Alphabet
import           Data.BitMatrix
import           Data.Bits
import           Data.BitVector        hiding (foldr)
import           Data.Matrix.NotStupid        (getRow, fromLists, setElem)
import           Data.MonoTraversable
import qualified Data.Vector                                               as V
import           Test.Custom.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding ((.?.), (.&.))
import           Debug.Trace

standardAlph :: Alphabet String
standardAlph =  constructAlphabet $ V.fromList ["A", "C", "G", "T", "-"]

sampleMeta :: CharacterMetadata DynamicChar             
sampleMeta =  CharMeta DirectOptimization standardAlph "" False False 1 mempty (constructDynamic [], constructDynamic []) 0 (GeneralCost 1 1)

-- This is needed to align AC(GT)(AT) with ACT. Taked from Wheeler '96, fig. 2, HTU just below root.
matrixForTesting :: DOAlignMatrix BV
matrixForTesting =  trace (show finalMatrix) $ finalMatrix
    where
        initMatrix = Data.Matrix.NotStupid.fromLists cellList
        cellList = [ [(0, LeftArrow, bitVec 5 1), (1, LeftArrow, bitVec 5 2), (2, LeftArrow, bitVec 5 24), (3, LeftArrow, bitVec 5 17)]
                   , [(1, LeftArrow, bitVec 5 1), (0, LeftArrow, bitVec 5 0), (2, LeftArrow, bitVec 5 0),  (3, LeftArrow, bitVec 5 0)]
                   ]
        finalMatrix = initMatrix

testSuite :: TestTree
testSuite =  testGroup "DO functionality" [ alignDOProperties
                                          , getSubCharsTest
                                          , overlapTest
                                          , getCostTest
                                          ]

alignDOProperties :: TestTree
alignDOProperties = testGroup "Properties of DO alignment algorithm" [ firstRow
                                                                     ]
    where
        firstRow = testProperty "First row of DO alignment matrix has expected directions" checkRow
            where
                checkRow :: DynamicChar -> Bool
                checkRow inSeq = fDir == DiagArrow && allLeft (V.tail result) && V.length result == (rowLen + 1)
                    where
                        rowLen  = olength inSeq
                        fullMat = createDOAlignMatrix inSeq inSeq (getCosts sampleMeta)
                        result  = getRow 0 fullMat
                        (_, fDir, _) = V.head result
                        allLeft = V.all (\(_, val, _) -> val == LeftArrow)

getSubCharsTest :: TestTree
getSubCharsTest  = testGroup "getSubChars tests" [ orTest
                                                 , lengthTest
                                                 , allReturnedCharsRightLength 
                                                 , posIsCorrect
                                                 ]
    where
        orTest = testProperty "Or-ing all returned static chars == input" f 
            where 
                f :: BitVector -> Bool
                f inChar = inChar == outChar
                    where
                        outChar = foldr (\(_, char) acc -> char .|. acc) (bitVec 0 0) (getSubChars inChar)
                        -- nada    = inChar `xor` inChar
        lengthTest = testProperty "Number of returned static chars == number of set bits in input" f
            where
                f :: BitVector -> Bool
                f inChar = popCount inChar == length (getSubChars inChar)
        allReturnedCharsRightLength = testProperty "All returned static chars are correct length" f
            where
                f :: BitVector -> Bool
                f inChar = correctLength
                    where
                        correctLength = foldr (\(_, char) acc -> (bitSizeMaybe char == len) && acc) True (getSubChars inChar)
                        len           = bitSizeMaybe inChar
        allReturnedCharsOnlyOneBitSet = testProperty "All returned static chars have only a single bit set" f
            where
                f :: BitVector -> Bool
                f inChar = onlyOneBit
                    where
                        onlyOneBit = foldr (\(_, char) acc -> (popCount char == 1) && acc) True (getSubChars inChar)
        posIsCorrect = testProperty "Position matches index of set bit in each returnd char" f
            where
                f :: BitVector -> Bool
                f inChar = rightPos
                    where
                        rightPos = foldr (\(pos, char) acc -> (testBit char pos) && acc) True (getSubChars inChar)

getCostTest :: TestTree
getCostTest = testGroup "Properties of getCosts" [ -- tcmTest 
                                                   generalCostTest
                                                  ]
    where
        {- assuming TCM is properly tested, and since getSubChars is right, we don't need to test with tcms
           Think about this with an assymetric TCM use case.
        tcmTest :: Bool
        tcmTest = allPassed
            where
                allPassed = foldr ()
        -}
        generalCostTest = testGroup "Works with a general cost structure" [ indelTest
                                                                          , subsTest 
                                                                          ]
            where 
                -- in both following tests, check both orders of characters
                indelTest = testProperty "Indels work correctly" f
                    where
                        f :: Bool
                        f = getCost costStruct char1 char2 == expectedResult && getCost costStruct char2 char1 == expectedResult
                        char1          = (4, setBit (bitVec 5 0) 4) -- has a gap
                        char2          = (0, setBit (bitVec 5 0) 2) -- no gap
                        expectedResult = (bitVec 5 20, 2)
                subsTest  = testProperty "Subs work correctly" f
                    where
                        f :: Bool
                        f = getCost costStruct char1 char2 == expectedResult && getCost costStruct char2 char1 == expectedResult
                        char1          = (3, setBit (bitVec 5 0) 3)
                        char2          = (0, setBit (bitVec 5 0) 0)
                        expectedResult = ((snd char1) .|. (snd char2), 1)
                costStruct = GeneralCost 2 1


{-
allPossibleCombosCostsTest :: TestTree
allPossibleCombosCostsTest = testProperty "allPossibleCombosCosts returns correct costs (getSubChars tested separately)" f
    where
        f :: Bool
        f = 
-}

overlapTest :: TestTree
overlapTest = testGroup "Overlap test cases" [ singleIntersectionTest
                                             , multipleIntersectionTest
                                             , unionTestWithGeneral
                                             ]
    where
        -- Withour loss of generality, next two use only General CostStructure
        singleIntersectionTest = testProperty "Given characters with single intersection, gives expected results" f
            where
                f :: Bool
                f =  expectedResult == result
                char1          = (bitVec 5 5) -- This is two bits on
                char2          = (bitVec 5 6) -- This is two different, but overlapping, bits set
                expectedResult = (char1 .&. char2, 0) -- Couldn't get it to compile with prefix notation.
                result         = getOverlap char1 char2 (GeneralCost 2 1)
        multipleIntersectionTest = testProperty "Given characters with single intersection, gives expected results" f
            where 
                f :: Bool
                f =  expectedResult == result
                char1          = (bitVec 5 5) -- This is two bits on
                char2          = (bitVec 5 7) -- This is two different, but overlapping, bits set
                expectedResult = (bitVec 5 5, 0)
                result         = getOverlap char1 char2 (GeneralCost 2 1)
        unionTestWithGeneral = testGroup "Given characters with no intersection and general cost, gives expected results" [ withoutGap
                                                                                                                          , withGap 
                                                                                                                          ]
            where
                withoutGap = trace (show result) $ testProperty "Without a gap" f
                    where
                        f :: Bool
                        f =  expectedResult == result
                        char1          = (bitVec 5 2) -- This is one bit on.
                        char2          = (bitVec 5 4) -- No gap.
                        expectedResult = (bitVec 5 6, 1)
                        result         = getOverlap char1 char2 (GeneralCost 2 1)
                withGap    = testProperty "With a gap" f
                    where
                        f :: Bool
                        f =  expectedResult == result
                        char1          = (bitVec 5 4)  -- This is one bit on.
                        char2          = (bitVec 5 18) -- Has a gap.
                        expectedResult = ((bitVec 5 6), 1)
                        result         = getOverlap char1 char2 (GeneralCost 2 1)
        unionTestWithTCM = testProperty "Given characters with no intersection and TCM, gives expected results" f
            where
                f :: Bool
                f = expectedResult == result
                char1          = (bitVec 5 2) -- This is one bit on.
                char2          = (bitVec 5 4) -- No gap.
                expectedResult = (bitVec 5 6, 1)
                result         = getOverlap char1 char2 (GeneralCost 2 1)

-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 2, bitVec 5 1, bitVec 5 2, bitVec 5 3]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 3]) (GeneralCost 2 1)
-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 2, bitVec 5 1, bitVec 5 12, bitVec 5 3]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 3]) (GeneralCost 2 1)
-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 12, bitVec 5 9]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 4]) (GeneralCost 2 1)



