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
import           Bio.Character.Encodable
--import           Bio.Character.Parsed
import           Bio.Metadata          hiding (alphabet)
--import           Bio.Metadata.Internal
import           Data.Alphabet
--import           Data.BitMatrix
import           Data.Bits
import           Data.BitVector        hiding (and, foldr)
import qualified Data.List.NonEmpty    as NE
import           Data.Matrix.NotStupid        (getRow, fromLists)
import           Data.MonoTraversable
import qualified Data.Vector           as V
import           Test.Custom
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding ((.&.))
import           Debug.Trace

standardAlph :: Alphabet String
standardAlph =  fromSymbols $ V.fromList ["A", "C", "G", "T", "-"]

sampleMeta :: CharacterMetadata DynamicChar             
sampleMeta =  CharMeta DirectOptimization standardAlph "" False False 1 mempty (constructDynamic [], constructDynamic []) 0 uniformCostStructure

defaultCostStructure :: CostStructure
defaultCostStructure = GeneralCost 2 1

uniformCostStructure :: CostStructure
uniformCostStructure = GeneralCost 1 1

alphabet :: Alphabet String
alphabet = fromSymbols $ pure <$> "ACGT-"

makeElem :: [String] -> DynamicCharacterElement
makeElem = encodeElement alphabet . NE.fromList

-- This is needed to align AC(GT)(AT) with ACT. Taked from Wheeler '96, fig. 2, HTU just below root.
matrixForTesting :: DOAlignMatrix BV
matrixForTesting =  trace (show finalMatrix) finalMatrix
    where
        initMatrix = Data.Matrix.NotStupid.fromLists cellList
        cellList = [ [(0, LeftArrow, bitVec 5 (1::Int)), (1, LeftArrow, bitVec 5 (2::Int)), (2, LeftArrow, bitVec 5 (24::Int)), (3, LeftArrow, bitVec 5 (17::Int))]
                   , [(1, LeftArrow, bitVec 5 (1::Int)), (0, LeftArrow, bitVec 5 (0::Int)), (2, LeftArrow, bitVec 5 (0::Int)),  (3, LeftArrow, bitVec 5 (0::Int))]
                   ]
        finalMatrix = initMatrix

testSuite :: TestTree
testSuite =  testGroup "DO functionality"
    [ directOptimizationProperties
    , alignDOProperties
    , getSubCharsTest
    , getCostTest
    , overlapTest
    ]

directOptimizationProperties :: TestTree
directOptimizationProperties = testGroup "General properties of direct optimization"
    [ identicalInputAndOutput
    , equalLengthPariwiseAlignments
    , nonDecreasingLengths
    ]
  where
    identicalInputAndOutput = testProperty "Identical input characters and uniform transition costs result in identity alignments" f
      where
        f :: DynamicChar -> Bool
        f char = and [ cost == 0
                     , derivedUngapped == filterGaps char
                     , derivedGapped   == char
                     , leftAlignment   == char
                     , rightAlignment  == char
                     ]
          where
            (derivedUngapped, cost, derivedGapped, leftAlignment, rightAlignment) = naiveDO char char uniformCostStructure

    equalLengthPariwiseAlignments = testProperty "Resulting alignments are all of same length" f
      where
        f :: Gen Bool
        f = do
            [char1,char2] <- take 2 <$> arbitraryDynamicCharStream
            let (_, _, derivedAlignment, leftAlignment, rightAlignment) = naiveDO char1 char2 defaultCostStructure
            pure $ olength leftAlignment == olength rightAlignment && olength rightAlignment == olength derivedAlignment

    nonDecreasingLengths = testProperty "Resulting alignments are of greater than or equal length to input characters" f
      where
        f :: Gen Bool
        f = do
            [char1,char2] <- take 2 <$> arbitraryDynamicCharStream
            let (_, _, derivedAlignment, leftAlignment, rightAlignment) = naiveDO char1 char2 defaultCostStructure
            pure $ all (uncurry (>=))
                 [ (outputLength, inputLength)
                 |  outputLength <- [olength leftAlignment, olength rightAlignment, olength derivedAlignment]
                 ,  inputLength  <- [olength char1, olength char2]
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
getSubCharsTest  = testGroup "getSubChars tests"
    [ orTest
    , lengthTest
    , allReturnedCharsRightLength 
    , allReturnedCharsOnlyOneBitSet
    , posIsCorrect
    ]
  where
    orTest = testProperty "Or-ing all returned static chars == input" f 
      where 
        f :: DynamicCharacterElement -> Bool
        f inChar = inChar == outChar
          where
            outChar = ofoldr (\(_, e) acc -> e .|. acc) zeroBits $ getSubChars inChar

    lengthTest = testProperty "Number of returned static chars == number of set bits in input" f
       where
         f :: DynamicCharacterElement -> Bool
         f inChar = popCount inChar == length (getSubChars inChar)

    allReturnedCharsRightLength = testProperty "All returned static chars are correct length" f
      where
        f :: DynamicCharacterElement -> Bool
        f inChar = all (\(_, e) -> bitSizeMaybe e == len) $ getSubChars inChar
          where
            len = bitSizeMaybe inChar

    allReturnedCharsOnlyOneBitSet = testProperty "All returned static chars have only a single bit set" f
      where
        f :: DynamicCharacterElement -> Bool
        f inChar = all (\(_,e) -> popCount e == 1) $ getSubChars inChar

    posIsCorrect = testProperty "Position matches index of set bit in each returnd char" f
      where
        f :: DynamicCharacterElement -> Bool
        f inChar = all (\(i, e) -> e `testBit` i) $ getSubChars inChar


getCostTest :: TestTree
getCostTest = testGroup "Properties of getCosts"
    [ -- tcmTest 
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
    generalCostTest = testGroup "Works with a general cost structure"
        [ indelTest
        , subsTest 
        ]
      where 
        -- in both following tests, check both orders of characters
        indelTest = testCase "Indels work correctly"
          . assertEqual "" expectedResult $ getCost defaultCostStructure char1 char2 
          where
            char1          = (4, makeElem ["-"]) -- has a gap
            char2          = (2, makeElem ["G"]) -- no gap
            expectedResult = (makeElem ["G","-"], 2)

        subsTest  = testCase "Subs work correctly"
          . assertEqual "" expectedResult $ getCost defaultCostStructure char1 char2
          where
            char1          = (3, makeElem ["T"])
            char2          = (0, makeElem ["A"])
            expectedResult = (snd char1 .|. snd char2, 1)

{-
allPossibleCombosCostsTest :: TestTree
allPossibleCombosCostsTest = testProperty "allPossibleCombosCosts returns correct costs (getSubChars tested separately)" f
    where
        f :: Bool
        f = 
-}

overlapTest :: TestTree
overlapTest = testGroup "Overlap test cases"
    [ singleIntersectionTest
    , multipleIntersectionTest
    , unionTestWithGeneral
    ]
  where
    -- Withour loss of generality, next two use only General CostStructure
    singleIntersectionTest = testCase "Given characters with single intersection, gives expected results"
        $ assertEqual "" expectedResult result
      where
        char1          = makeElem ["A","G"]   -- This is two bits on
        char2          = makeElem ["C","G"]   -- This is two different, but overlapping, bits set
        expectedResult = (char1 .&. char2, 0) -- Couldn't get it to compile with prefix notation.
        result         = getOverlap char1 char2 defaultCostStructure

    multipleIntersectionTest = testCase "Given characters with single intersection, gives expected results"
        $ assertEqual "" expectedResult result
      where 
        char1          =  makeElem ["A","G"]     -- This is two bits on
        char2          =  makeElem ["A","C","G"] -- This is two different, but overlapping, bits set
        expectedResult = (makeElem ["A","G"], 0)
        result         = getOverlap char1 char2 defaultCostStructure
                
    unionTestWithGeneral = testGroup "Given characters with no intersection and general cost, gives expected results"
        [ withoutGap
        , withGap 
        ]
      where
        withoutGap = testCase "Without a gap"
            $ assertEqual "" expectedResult result
          where
            char1          =  makeElem ["C"] -- This is one bit on.
            char2          =  makeElem ["G"] -- No gap.
            expectedResult = (makeElem ["C","G"], 1)
            result         = getOverlap char1 char2 defaultCostStructure
            
        withGap    = testCase "With a gap"
            $ assertEqual "" expectedResult result
          where
            char1          =  makeElem ["G"]     -- This is one bit on.
            char2          =  makeElem ["C","-"] -- Has a gap.
            expectedResult = (makeElem ["C","G"], 1)
            result         = getOverlap char1 char2 defaultCostStructure
{-                        
        unionTestWithTCM = testProperty "Given characters with no intersection and TCM, gives expected results" f
            where
                f :: Bool
                f = expectedResult == result
                char1          =  bitVec 5 (2::Int) -- This is one bit on.
                char2          =  bitVec 5 (4::Int) -- No gap.
                expectedResult = (bitVec 5 (6::Int), 1)
                result         = getOverlap char1 char2 (GeneralCost 2 1)
-}

-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 2, bitVec 5 1, bitVec 5 2, bitVec 5 3]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 3]) (GeneralCost 2 1)
-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 2, bitVec 5 1, bitVec 5 12, bitVec 5 3]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 3]) (GeneralCost 2 1)
-- createDOAlignMatrix (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 12, bitVec 5 9]) (DC $ Data.BitMatrix.fromRows [bitVec 5 1, bitVec 5 2, bitVec 5 4]) (GeneralCost 2 1)



