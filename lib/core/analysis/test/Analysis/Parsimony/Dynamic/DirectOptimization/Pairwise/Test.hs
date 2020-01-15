-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynmaic.DirectOptimization.Pairwise.Test
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test
  ( testSuite
  ) where


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
{-
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen
-}
import           Bio.Character.Encodable
import           Data.Alphabet
import           Data.List                                              (intercalate)
import           Data.List.NonEmpty                                     (NonEmpty (..))
import           Data.MonoTraversable
import           Data.TCM.Dense
import           Data.TCM.Memoized
import           Test.Custom.NucleotideSequence
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck                                  as SC


testSuite :: TestTree
testSuite = testGroup "Pairwise alignment tests"
    [ testSuiteNaiveDO
    , testSuiteMemoizedDO
    , testSuiteUkkonnenDO
    , testSuiteForeignDO
    , constistentImplementation
    ]


constistentImplementation :: TestTree
constistentImplementation = testGroup "All implementations return same states"
    [ consistentResults "Consistenty over discrete metric" discreteMetric
    , consistentResults "Consistenty over L1 norm" l1Norm
    , consistentResults "Consistenty over prefer substitution metric (1:2)" preferSubMetric
    , consistentResults "Consistenty over prefer insertion/deletion metric (2:1)" preferGapMetric
    ]


consistentResults :: String -> (Word -> Word -> Word) -> TestTree
consistentResults testLabel metric = SC.testProperty testLabel $ SC.forAll checkConsistency
  where
    dense  = genDenseMatrix metric
    memoed = getMedianAndCost2D (genMemoMatrix metric)
    median x y = fst $ memoed x y
    f :: DynamicCharacterElement -> DynamicCharacter
    f = constructDynamic . (:|[])

    checkConsistency :: (NucleotideBase, NucleotideBase) -> Either String String
    checkConsistency p@(NB x, NB y)  
      | naiveResult == memoedResult && naiveResult == foreignResult = Right $ show p
      | otherwise = Left errorMessage
      where
        naiveResult   = naiveDO           (f x) (f y) metric
        memoedResult  = naiveDOMemo       (f x) (f y) memoed
        foreignResult = foreignPairwiseDO (f x) (f y) dense
        errorMessage  = unlines
                   [ ""
                   , unwords [ "Naive:  ", showResult median   naiveResult, show   naiveResult ]
                   , unwords [ "Memoed: ", showResult median  memoedResult, show  memoedResult ]
                   , unwords [ "Foreign:", showResult median foreignResult, show foreignResult ]
                   ]


showResult
  :: (AmbiguityGroup -> AmbiguityGroup -> AmbiguityGroup)
  -> (Word, DynamicCharacter)
  -> String
showResult transformation (cost, a) = (\x->"("<>x<>")") $ intercalate ","
    [ show cost
    , renderDynamicCharacter alphabet transformation a
    ]


testSuiteNaiveDO :: TestTree
testSuiteNaiveDO = testGroup "Naive DO"
    [ isValidPairwiseAlignment "Naive DO over discrete metric"
       $ \x y -> naiveDO x y discreteMetric
    , isValidPairwiseAlignment "Naive DO over L1 norm"
       $ \x y -> naiveDO x y l1Norm
    , isValidPairwiseAlignment "Naive DO over prefer substitution metric (1:2)"
       $ \x y -> naiveDO x y preferSubMetric
    , isValidPairwiseAlignment "Naive DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> naiveDO x y preferGapMetric
    ]


testSuiteMemoizedDO :: TestTree
testSuiteMemoizedDO = testGroup "Memoized DO"
    [ isValidPairwiseAlignment "Memoized DO over discrete metric"
       $ \x y -> naiveDOMemo x y (getMedianAndCost2D (genMemoMatrix discreteMetric))
    , isValidPairwiseAlignment "Memoized DO over L1 norm"
       $ \x y -> naiveDOMemo x y (getMedianAndCost2D (genMemoMatrix l1Norm))
    , isValidPairwiseAlignment "Memoized DO over prefer substitution metric (1:2)"
       $ \x y -> naiveDOMemo x y (getMedianAndCost2D (genMemoMatrix preferSubMetric))
    , isValidPairwiseAlignment "Memoized DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> naiveDOMemo x y (getMedianAndCost2D (genMemoMatrix preferGapMetric))
    ]


testSuiteUkkonnenDO :: TestTree
testSuiteUkkonnenDO = testGroup "Ukkonnen DO"
    [ isValidPairwiseAlignment "Ukkonnen DO over discrete metric"
       $ \x y -> ukkonenDO x y (getMedianAndCost2D (genMemoMatrix discreteMetric))
    , isValidPairwiseAlignment "Ukkonnen DO over L1 norm"
       $ \x y -> ukkonenDO x y (getMedianAndCost2D (genMemoMatrix l1Norm))
    , isValidPairwiseAlignment "Ukkonnen DO over prefer substitution metric (1:2)"
       $ \x y -> ukkonenDO x y (getMedianAndCost2D (genMemoMatrix preferSubMetric))
    , isValidPairwiseAlignment "Ukkonnen DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> ukkonenDO x y (getMedianAndCost2D (genMemoMatrix preferGapMetric))
    ]


testSuiteForeignDO :: TestTree
testSuiteForeignDO = testGroup "Foreign C DO"
    [ isValidPairwiseAlignment "Foreign C DO over discrete metric"
       $ \x y -> foreignPairwiseDO x y (genDenseMatrix discreteMetric)
    , isValidPairwiseAlignment "Foreign C DO over L1 norm"
       $ \x y -> foreignPairwiseDO x y (genDenseMatrix l1Norm)
    , isValidPairwiseAlignment "Foreign C DO over prefer substitution metric (1:2)"
       $ \x y -> foreignPairwiseDO x y (genDenseMatrix preferSubMetric)
    , isValidPairwiseAlignment "Foreign C DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> foreignPairwiseDO x y (genDenseMatrix preferGapMetric)
    ]


{-
isValidPairwiseAlignment
  :: DOCharConstraint s
  => String
  -> (s -> s -> (Word, s, s, s, s))
  -> TestTree
-}
isValidPairwiseAlignment
  :: String
  -> (DynamicCharacter -> DynamicCharacter -> (Word, DynamicCharacter))
  -> TestTree
isValidPairwiseAlignment testLabel alignmentFunction = testGroup testLabel
    [  testProperty "alignment function is commutative"               commutivity
     , testProperty "output length is >= input length"                greaterThanOrEqualToInputLength
     , testProperty "alignment length is =< sum of input lengths"     totalAlignmentLengthLessThanOrEqualToSumOfLengths
--     , testProperty "output alignments were not erroneously swapped"  outputsCorrespondToInputs
--     , testProperty "output alignments were not erroneously reversed" outputsAreNotReversed
--     , testProperty "output alignments only contain new gaps"         filterGapsEqualsInput
    ]
  where
    commutivity :: (NucleotideSequence, NucleotideSequence) -> Property
    commutivity _input@(NS lhs, NS rhs) =
        let x@(a,b) = alignmentFunction lhs rhs
            y@(c,d) = alignmentFunction rhs lhs
        in  x === (c, omap swapContext d) .&&. (a, omap swapContext b) === y

    greaterThanOrEqualToInputLength :: (NucleotideSequence, NucleotideSequence) -> Bool
    greaterThanOrEqualToInputLength (NS lhs, NS rhs) =
        olength med >= olength lhs && olength med >= olength rhs
      where
        (_, med) = alignmentFunction lhs rhs

    totalAlignmentLengthLessThanOrEqualToSumOfLengths :: (NucleotideSequence, NucleotideSequence) -> Property
    totalAlignmentLengthLessThanOrEqualToSumOfLengths (NS lhs, NS rhs) =
        counterexample shownCounterexample $ medLen <= lhsLen + rhsLen
      where
        (_, med) = alignmentFunction lhs rhs
        medLen = olength med
        lhsLen = olength lhs
        rhsLen = olength rhs
        shownCounterexample = unwords [ show medLen, ">", show lhsLen, "+", show rhsLen ]

{-
    outputsCorrespondToInputs :: (NucleotideSequence, NucleotideSequence) -> Property
    outputsCorrespondToInputs (NS lhs, NS rhs) =
        lhs /= rhs && filterGaps lhs' /= filterGaps rhs' ==>
            counterexample "lhs' === rhs" (filterGaps lhs' /= filterGaps rhs) .&&.
            counterexample "rhs' === lhs" (filterGaps rhs' /= filterGaps lhs)
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

    outputsAreNotReversed :: (NucleotideSequence, NucleotideSequence) -> Property
    outputsAreNotReversed (NS lhs, NS rhs) = --trace ("lhs:  "   <> fmap (\x -> if x == '\n' then ' ' else x) (show $ filterGaps lhs)            <>
                                                   --  "\nlhs': " <> fmap (\x -> if x == '\n' then ' ' else x) (show $  lhs') <>
                                                   --  "\nrhs:  " <> fmap (\x -> if x == '\n' then ' ' else x) (show $  rhs)            <>
                                                   --  "\nrhs': " <> fmap (\x -> if x == '\n' then ' ' else x) (show $  rhs') <> "\n"
                                                   -- ) $
        counterexample (show lhs <> show lhs') (isNotPalindrome lhs ==> isNotReversed (filterGaps lhs') lhs) .&&.
        counterexample (show rhs <> show rhs') (isNotPalindrome rhs ==> isNotReversed (filterGaps rhs') rhs)
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs
        isNotReversed x y = reverse (otoList x) /= otoList y
        isNotPalindrome x = reverse (otoList x) /= otoList x

    filterGapsEqualsInput :: (NucleotideSequence, NucleotideSequence) -> Property
    filterGapsEqualsInput (NS lhs, NS rhs) =
        filterGaps lhs' === filterGaps lhs .&&. filterGaps rhs' === filterGaps rhs
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

-}


genDenseMatrix :: (Word -> Word -> Word) -> DenseTransitionCostMatrix
genDenseMatrix = generateDenseTransitionCostMatrix 0  5


genMemoMatrix :: (Word -> Word -> Word) -> MemoizedCostMatrix
genMemoMatrix  = generateMemoizedTransitionCostMatrix 5


discreteMetric :: (Ord a, Num a) => a -> a -> a
discreteMetric i j = if i /= j then 1 else 0


l1Norm :: (Ord a, Num a) => a -> a -> a
l1Norm i j = max i j - min i j


preferGapMetric :: (Ord a, Num a) => a -> a -> a
preferGapMetric i j
  | i == j    = 0
  | i == 4    = 1
  | j == 4    = 1
  | otherwise = 2


preferSubMetric :: (Ord a, Num a) => a -> a -> a
preferSubMetric i j
  | i == j    = 0
  | i == 4    = 2
  | j == 4    = 2
  | otherwise = 1


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"]

{-


standardAlph :: Alphabet String
standardAlph =  fromSymbols $ V.fromList ["A", "C", "G", "T", "-"]


sampleMeta :: CharacterMetadata DynamicCharacter
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
        f :: DynamicCharacter -> Bool
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
            [char1,char2] <- take 2 <$> arbitraryDynamicCharacterStream
            let (_, _, derivedAlignment, leftAlignment, rightAlignment) = naiveDO char1 char2 defaultCostStructure
            pure $ olength leftAlignment == olength rightAlignment && olength rightAlignment == olength derivedAlignment

    nonDecreasingLengths = testProperty "Resulting alignments are of greater than or equal length to input characters" f
      where
        f :: Gen Bool
        f = do
            [char1,char2] <- take 2 <$> arbitraryDynamicCharacterStream
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
                checkRow :: DynamicCharacter -> Bool
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

    posIsCorrect = testProperty "Position matches index of set bit in each returned char" f
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
           Think about this with an asymmetric TCM use case.
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

-}
