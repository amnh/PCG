-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Unit tests for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Analysis.ImpliedAlignment.Test where

import           Analysis.Parsimony.Binary.Internal
import           Analysis.Parsimony.Binary.Optimization
import           Analysis.Parsimony.Binary.DirectOptimization
import           Analysis.ImpliedAlignment.Internal
import           Analysis.ImpliedAlignment.Standard
import           Analysis.ImpliedAlignment.DynamicProgramming
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.Metadata
import           Bio.PhyloGraph            hiding (name)
import           Bio.PhyloGraph.Network           (nodeIsLeaf)
import           Bio.PhyloGraph.Node.ImpliedAlign (getHomologies')
import           Data.Alphabet
import           Data.BitVector          (BitVector, setBit, bitVec)
import           Data.Foldable
import           Data.Function           (on)
import qualified Data.IntMap       as IM
import           Data.IntSet             (IntSet)
import           Data.List
import           Data.MonoTraversable
import qualified Data.Set          as S
import           Data.Vector             (Vector)
import qualified Data.Vector       as V
import           Test.Custom
--import qualified Test.Custom.Types as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances

import Debug.Trace

sillyMeta :: Vector (CharacterMetadata s)
sillyMeta = pure CharMeta
          { charType   = DirectOptimization
          , alphabet   = constructAlphabet []
          , name       = "DefaultCharacter"
          , isAligned  = False
          , isIgnored  = False
          , weight     = 1.0
          , stateNames = mempty
          , fitchMasks = undefined
          , rootCost   = 0.0
          , costs      = GeneralCost { indelCost = 1, subCost = 4 }
          }

performImpliedAlignment = deriveImpliedAlignments defMeta . allOptimization 1 defMeta

testSuite :: TestTree
testSuite = testGroup "Implied Alignment"
          [ testNumerate
          , testImpliedAlignmentCases
--          , fullIA
          ]

fullIA :: TestTree
fullIA = testGroup "Full alignment properties"
    [ lenHoldsTest
    ]
  where
    lenHoldsTest = testProperty "The sequences on a tree are longer or the same at end." checkLen

    checkLen :: SimpleTree -> Bool
    checkLen inputTree = nodeInvariantHolds impliedAlignmentLengthIsLonger outputTree
      where
        outputTree = performImpliedAlignment inputTree
        impliedAlignmentLengthIsLonger node = and $ V.zipWith ((<=) `on` olength) nodeImpliedAlignements nodeFinalCharacters
          where
            nodeImpliedAlignements = getHomologies' node
            nodeFinalCharacters    = getFinalGapped node

testNumerate :: TestTree
testNumerate = testGroup "Numeration properties"
      [ idHolds
      , lengthHolds
      , counterIncrease
      , monotonic
      ]
    where
        idHolds                          = testProperty "When a sequence is numerated with itself, get indices and the same counter" checkID
        checkID :: DynamicChar -> Bool
        checkID inChar                   = onull inChar || (traces == defaultH && counter <= olength inChar)
            where
                defaultH = V.fromList [0..olength inChar - 1] 
                (traces, (_, counter), _) =  numerateOne inChar inChar (0, 0)

        -- TODO: Talk to Eric about olength ()
        lengthHolds                      = testProperty "Numerate returns a sequence of the correct length" checkLen
        checkLen :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkLen inParse count           = V.length traces >= maxLen
            where 
                (seq1, seq2)              = encodeArbSameLen inParse
                (traces, (_, counter), _) = numerateOne seq1 seq2 (olength seq1, count)
                maxLen                    = maximum [olength seq1, olength seq2]

        counterIncrease                   = testProperty "After numerate runs, counter is same or larger" checkCounter
        checkCounter :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkCounter inParse count        = counter >= count
            where 
                (seq1, seq2)              = encodeArbSameLen inParse
                (traces, (_, counter), _) = numerateOne seq1 seq2 (olength seq1, count)
        monotonic = testProperty "Numerate produces a monotonically increasing homology" checkIncrease
        checkIncrease :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkIncrease inParse count       = increases $ toList traces
            where 
                (seq1, seq2)         = encodeArbSameLen inParse
                (traces, counter, _) = numerateOne seq1 seq2 (olength seq1, count)
                increases :: Ord a => [a] -> Bool
                increases []         = True
                increases [x]        = True
                increases (x:y:xs)   = x < y && increases (y:xs)
{-
-- | Wrapper function to start and then terminate an IA numeration
partNumerate :: DAG -> Node -> Vector m -> Counts -> Node -> (Counts, t)
partNumerate inTree curNode inMeta curCounts stopNode
    | (code curNode) == (code stopNode) = (curCounts, inTree)
    | otherwise = partNumerate -}

testAdjacentDeletionInsertionEvents2 :: TestTree
testAdjacentDeletionInsertionEvents2 = testCase "Pair of adjacent insertion & deletion events (insertion should be first)" $ testHarness tree
      where
        testHarness    = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" transformation getHomologies'
        transformation = deriveImpliedAlignments sillyMeta . allOptimization 1 sillyMeta
        tree = [ ( 0, ""     , [""      , ""      ], [ 1, 2])
               , ( 1, "ACTAA", ["A-CTAA", "AC-TAA"], []     )
               , ( 2, ""     , [""      , ""      ], [ 3, 4])
               , ( 3, "ACTAA", ["A-CTAA", "AC-TAA"], []     )
               , ( 4, ""     , [""      , ""      ], [ 5, 6])
               , ( 5, "ATTA" , ["AT-T-A", "A-TT-A"], []     )
               , ( 6, "ATTA" , ["AT-T-A", "A-TT-A"], []     )
               ]

testAdjacentDeletionInsertionEvents :: TestTree
testAdjacentDeletionInsertionEvents = testCase "Pair of adjacent insertion & deletion events" $ testHarness tree
      where
        testHarness    = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" transformation getHomologies'
        transformation = deriveImpliedAlignments sillyMeta . allOptimization 1 sillyMeta
        tree = [ ( 0, ""   , [""    , ""    ], [ 1, 2])
               , ( 1, "ACA", ["A-CA", "AC-A"], []     )
               , ( 2, ""   , [""    , ""    ], [ 3, 4])
               , ( 3, "ACA", ["A-CA", "AC-A"], []     )
               , ( 4, ""   , [""    , ""    ], [ 5, 6])
               , ( 5, "ATA", ["AT-A", "A-TA"], []     )
               , ( 6, "ATA", ["AT-A", "A-TA"], []     )
               ]


testDeletedInsertionAntisymetry = testCase "Deleted insertion events anti-symetrically reflected across the root" $ decorationTest tree
      where
        tree = [ ( 0, ""     , [""     ], [ 1, 2])
               , ( 1, ""     , [""     ], [ 3,16])
               , ( 2, ""     , [""     ], [ 4,17])
               , ( 3, ""     , [""     ], [ 5,15])
               , ( 4, ""     , [""     ], [ 6,18])
               , ( 5, ""     , [""     ], [ 7,14])
               , ( 6, ""     , [""     ], [ 8,19])
               , ( 7, ""     , [""     ], [ 9,13])
               , ( 8, ""     , [""     ], [10,20])
               , ( 9, ""     , [""     ], [11,12])
               , (10, ""     , [""     ], [21,22])
               , (11, "ACAA" , ["AC-A--A", "A-CA--A", "AC-A--A", "A-CA--A"], []     )
               , (12, "ACAA" , ["AC-A--A", "A-CA--A", "AC-A--A", "A-CA--A"], []     )
               , (13, "ACATA", ["AC-AT-A", "A-CAT-A", "AC-A-TA", "A-CA-TA"], []     )
               , (14, "ACATA", ["AC-AT-A", "A-CAT-A", "AC-A-TA", "A-CA-TA"], []     )
               , (15, "AAA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"], []     )
               , (16, "AAA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"], []     )
               , (17, "AAA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"], []     )
               , (18, "AAA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"], []     )
               , (19, "ACATA", ["A-CA-TA", "AC-A-TA", "A-CAT-A", "AC-AT-A"], []     )
               , (20, "ACATA", ["A-CA-TA", "AC-A-TA", "A-CAT-A", "AC-AT-A"], []     )
               , (21, "AATA" , ["A--A-TA", "A--A-TA", "A--AT-A", "A--AT-A"], []     )
               , (22, "AATA" , ["A--A-TA", "A--A-TA", "A--AT-A", "A--AT-A"], []     )
               ]

testDoubleDeletedInsertion = testCase "Double deletion event of an single insertion event" $ decorationTest tree
      where
        tree = [ ( 0, ""   , [""    , ""    ], [ 1, 2])
               , ( 1, "AA" , ["A--A", "A--A"], []     )
               , ( 2, ""   , [""    , ""    ], [ 3, 4])
               , ( 3, "AA" , ["A--A", "A--A"], []     )
               , ( 4, ""   , [""    , ""    ], [ 5, 6])
               , ( 5, "ATA", ["AT-A", "A-TA"], []     )
               , ( 6, ""   , [""    , ""    ], [ 7, 8])
               , ( 7, "ATA", ["AT-A", "A-TA"], []     )
               , ( 8, ""   , [""    , ""    ], [ 9,10])
               , ( 9, "ATA", ["AT-A", "A-TA"], []     )
               , (10, ""   , [""    , ""    ], [11,12])
               , (11, "AA" , ["A--A", "A--A"], []     )
               , (12, ""   , [""    , ""    ], [13,14])
               , (13, "AA" , ["A--A", "A--A"], []     )
               , (14, ""   , [""    , ""    ], [15,16])
               , (15, "AA" , ["A--A", "A--A"], []     )
               , (16, ""   , [""    , ""    ], [17,18])
               , (17, "ATA", ["A-TA", "AT-A"], []     )
               , (18, "ATA", ["A-TA", "AT-A"], []     )
               ]




testInsertedDeletions = testGroup "Insertion of deletion events"
    [ testInsertedDeletion
    , testInsertedDeletion2
    , testInsertedDeletion3
    ]
  where
    testInsertedDeletion = testCase "Insertion event of an deletion event" $ decorationTest tree
      where
        tree = [ ( 0, ""      , [""     , ""     , ""     ], [11,12])
               , ( 1, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , ( 2, ""      , [""     , ""     , ""     ], [3, 4])
               , ( 3, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , ( 4, ""      , [""     , ""     , ""     ], [5, 6])
               , ( 5, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , ( 6, ""      , [""     , ""     , ""     ], [7, 8])
               , ( 7, "AATT"  , ["AA-TT", "AAT-T", "AATT-"], []    ) -- Multiple valid gap locations here.
               , ( 8, ""      , [""     , ""     , ""     ], [9,10])
               , ( 9, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , (10, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , (11, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , (12, ""      , [""     , ""     , ""     ], [1, 2])
               ]

    testInsertedDeletion2 = testCase "Insertion event of an deletion event 2" $ decorationTest tree
      where
        tree = [ ( 0, ""      , [""     , ""     , ""     ], [1, 2])
               , ( 1, ""      , [""     , ""     , ""     ], [3, 4])
               , ( 2, ""      , [""     , ""     , ""     ], [5, 6])
               , ( 3, ""      , [""     , ""     , ""     ], [7, 8])
               , ( 4, "AATT"  , ["AA-TT", "AAT-T", "AATT-"], []    ) -- Multiple valid gap locations here.
               , ( 5, "AATT"  , ["AA-TT", "AAT-T", "AATT-"], []    ) -- Multiple valid gap locations here.
               , ( 6, ""      , [""     , ""     , ""     ], [9,10])
               , ( 7, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , ( 8, "AATTT" , ["AATTT", "AATTT", "AATTT"], [])
               , ( 9, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               , (10, "AATTT" , ["AATTT", "AATTT", "AATTT"], []    )
               ]


testInsertedDeletion3 = testCase "Insertion event of an deletion event 3" $ decorationTest tree
      where
        tree = [ ( 0, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [ 1, 4])
               , ( 1, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [ 2, 3])
               , ( 2, "AATTG", ["AA-TTG", "AA-TTG", "AAT-TG", "AAT-TG", "AATT-G", "AATT-G"], []     )
               , ( 3, "AATTG", ["AA-TTG", "AA-TTG", "AAT-TG", "AAT-TG", "AATT-G", "AATT-G"], []     )
               , ( 4, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [ 5, 6])
               , ( 5, "AATTG", ["AA-TTG", "AA-TTG", "AAT-TG", "AAT-TG", "AATT-G", "AATT-G"], []     )
               , ( 6, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [ 7, 8])
               , ( 7, "AATG" , ["AA--TG", "AA-T-G", "AA-TTG", "AAT--G", "AA-T-G", "AAT--G"], []     )
               , ( 8, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [ 9,10])
               , ( 9, "AATG" , ["AA--TG", "AA-T-G", "AA--TG", "AAT--G", "AA-T-G", "AAT--G"], []     )
               , (10, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [11,12])
               , (11, "AATG" , ["AA--TG", "AA-T-G", "AA--TG", "AAT--G", "AA-T-G", "AAT--G"], []     )
               , (12, ""     , [""      , ""      , ""      , ""      , ""      , ""      ], [13,14])
               , (13, "AATTG", ["AAT-TG", "AATT-G", "AA-TTG", "AATT-G", "AA-TTG", "AAT-TG"], []     )
               , (14, "AATTG", ["AAT-TG", "AATT-G", "AA-TTG", "AATT-G", "AA-TTG", "AAT-TG"], []     )
               ]

testDeletedInsertions = testGroup "Deletion of insertion events"
    [ testDeletedInsertionSingle
    , testDeletedInsertionGroupMiddle
    , testDeletedInsertionGroupPrepend
    , testDeletedInsertionGroupAppend
    , testDeletedInsertionAntisymetry
    , testDoubleDeletedInsertion
    ]

testDeletedInsertionSingle = testCase "Deletion event of an single insertion event" $ decorationTest tree
      where
        tree = [ ( 0, ""     , [""     ], [ 1, 2])
               , ( 1, "AATT" , ["AA-TT"], []     )
               , ( 2, ""     , [""     ], [ 3, 4])
               , ( 3, "AATT" , ["AA-TT"], []     )
               , ( 4, ""     , [""     ], [ 5, 6])
               , ( 5, "AATT" , ["AA-TT"], []     )
               , ( 6, ""     , [""     ], [ 7, 8])
               , ( 7, "AACTT", ["AACTT"], []     )
               , ( 8, ""     , [""     ], [ 9,10])
               , ( 9, "AACTT", ["AACTT"], []     )
               , (10, ""     , [""     ], [11,12])
               , (11, "AACTT", ["AACTT"], []     )
               , (12, ""     , [""     ], [13,14])
               , (13, "AATT" , ["AA-TT"], []     )
               , (14, "AATT" , ["AA-TT"], []     )
               ]

testDeletedInsertionGroupMiddle = testCase "Deletion event of an insertion event nested between other insertion events" $ decorationTest tree
      where
        tree = [ ( 0, ""       , [""       ], [ 1, 2])
               , ( 1, "AATT"   , ["AA---TT"], []     )
               , ( 2, ""       , [""       ], [ 3, 4])
               , ( 3, "AATT"   , ["AA---TT"], []     )
               , ( 4, ""       , [""       ], [ 5, 6])
               , ( 5, "AATT"   , ["AA---TT"], []     )
               , ( 6, ""       , [""       ], [ 7, 8])
               , ( 7, "AACGCTT", ["AACGCTT"], []     )
               , ( 8, ""       , [""       ], [ 9,10])
               , ( 9, "AACGCTT", ["AACGCTT"], []     )
               , (10, ""       , [""       ], [11,12])
               , (11, "AACGCTT", ["AACGCTT"], []     )
               , (12, ""       , [""       ], [13,14])
               , (13, "AACCTT" , ["AAC-CTT"], []     )
               , (14, "AACCTT" , ["AAC-CTT"], []     )
               ]
testDeletedInsertionGroupPrepend = testCase "Deletion event of an insertion event prepended to a group of other insertion events" $ decorationTest tree
      where
        tree = [ ( 0, ""       , [""       ], [ 1, 2])
               , ( 1, "AATT"   , ["AA---TT"], []     )
               , ( 2, ""       , [""       ], [ 3, 4])
               , ( 3, "AATT"   , ["AA---TT"], []     )
               , ( 4, ""       , [""       ], [ 5, 6])
               , ( 5, "AATT"   , ["AA---TT"], []     )
               , ( 6, ""       , [""       ], [ 7, 8])
               , ( 7, "AAGCCTT", ["AAGCCTT"], []     )
               , ( 8, ""       , [""       ], [ 9,10])
               , ( 9, "AAGCCTT", ["AAGCCTT"], []     )
               , (10, ""       , [""       ], [11,12])
               , (11, "AAGCCTT", ["AAGCCTT"], []     )
               , (12, ""       , [""       ], [13,14])
               , (13, "AACCTT" , ["AA-CCTT"], []     )
               , (14, "AACCTT" , ["AA-CCTT"], []     )
               ]

testDeletedInsertionGroupAppend = testCase "Deletion event of an insertion event appended to a group of other insertion events" $ decorationTest tree
      where
        tree = [ ( 0, ""       , [""       ], [ 1, 2])
               , ( 1, "AATT"   , ["AA---TT"], []     )
               , ( 2, ""       , [""       ], [ 3, 4])
               , ( 3, "AATT"   , ["AA---TT"], []     )
               , ( 4, ""       , [""       ], [ 5, 6])
               , ( 5, "AATT"   , ["AA---TT"], []     )
               , ( 6, ""       , [""       ], [ 7, 8])
               , ( 7, "AACCGTT", ["AACCGTT"], []     )
               , ( 8, ""       , [""       ], [ 9,10])
               , ( 9, "AACCGTT", ["AACCGTT"], []     )
               , (10, ""       , [""       ], [11,12])
               , (11, "AACCGTT", ["AACCGTT"], []     )
               , (12, ""       , [""       ], [13,14])
               , (13, "AACCTT" , ["AACC-TT"], []     )
               , (14, "AACCTT" , ["AACC-TT"], []     )
               ]

decorationTest :: Foldable t => t (Int, String, [String], [Int]) -> Assertion
decorationTest          = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" performImpliedAlignment getHomologies'

testImpliedAlignmentCases :: TestTree
testImpliedAlignmentCases = testGroup "Explicit test cases for implied alignment"
    [ testDeletedInsertions
    , testInsertedDeletions
    , testSimpleInsertionDeletionBiasing
    , testAdjacentDeletionInsertionEvents
    , testAdjacentDeletionInsertionEvents2
    ]
  where

    testSimpleInsertionDeletionBiasing = testGroup "Insertion & deletion event appending & prepending to character"
        [ testAppendedDeletions
        , testPrependedDeletions
        , testAppendedInsertions
        , testPrependedInsertions
        ]
      where
        testAppendedDeletions = testCase "Chain of deletions appended to sequence" $ decorationTest tree
          where
            tree = [ (0,     "", [    ""], [1,2])
                   , (1, "ACGT", ["ACGT"], []   )
                   , (2,     "", [    ""], [3,4])
                   , (3,  "ACG", ["ACG-"], []   )
                   , (4,     "", [    ""], [5,6])
                   , (5,   "AC", ["AC--"], []   )
                   , (6,    "A", ["A---"], []   )
                   ]

        testPrependedDeletions = testCase "Chain of deletions prepended to sequence" $ decorationTest tree
          where
            tree = [ (0,     "", [    ""], [1,2])
                   , (1, "TGCA", ["TGCA"], []   )
                   , (2,     "", [    ""], [3,4])
                   , (3,  "GCA", ["-GCA"], []   )
                   , (4,     "", [    ""], [5,6])
                   , (5,   "CA", ["--CA"], []   )
                   , (6,    "A", ["---A"], []   )
                   ]

testAppendedInsertions = testCase "Chain of insertions appended to sequence" $ decorationTest tree
          where
            tree = [ (0, ""    , [    ""], [1,2])
                   , (1, "A"   , ["A---"], []   )
                   , (2, ""    , [    ""], [3,4])
                   , (3, "AC"  , ["AC--"], []   )
                   , (4, ""    , [    ""], [5,6])
                   , (5, "ACG" , ["ACG-"], []   )
                   , (6, "ACGT", ["ACGT"], []   )
                   ]

testPrependedInsertions = testCase "Chain of insertions prepended to sequence" $ decorationTest tree
          where
            tree = [ (0, ""    , [    ""], [1,2])
                   , (1, "A"   , ["---A"], []   )
                   , (2, ""    , [    ""], [3,4])
                   , (3, "CA"  , ["--CA"], []   )
                   , (4, ""    , [    ""], [5,6])
                   , (5, "GCA" , ["-GCA"], []   )
                   , (6, "TGCA", ["TGCA"], []   )
                   ]

-- | Useful function to convert encoding information to two encoded seqs
encodeArbSameLen :: (GoodParsedChar, GoodParsedChar) -> (DynamicChar, DynamicChar)
encodeArbSameLen (parse1, parse2) = (encodeDynamic alph (V.take minLen p1), encodeDynamic alph (V.take minLen p2))
    where
        (p1,p2) = (getGoodness parse1, getGoodness parse2)
        minLen  = minimum [length p1, length p2]
        oneAlph = foldMap S.fromList
        alph    = constructAlphabet $ oneAlph p1 `S.union` oneAlph p2

-- | Newtyping ensures that the sequence and ambiguity groups are both non empty.
newtype GoodParsedChar
      = GoodParsedChar
      { getGoodness :: ParsedChar
      } deriving (Eq,Show)

instance Arbitrary GoodParsedChar where
  arbitrary = do
    symbols                     <- getNonEmpty <$> arbitrary :: Gen [String]
    let ambiguityGroupGenerator =  sublistOf symbols `suchThat` (not . null)
    someAmbiguityGroups         <- V.fromList <$> listOf1 ambiguityGroupGenerator
    pure $ GoodParsedChar someAmbiguityGroups



insertionDeletionTest :: Foldable t
                      => Int          -- ^ Root node reference
                      -> String       -- ^ Alphabet symbols
                      -> t (Int, String, [Int])
                      -> Int          -- ^ Parent node index
                      -> Int          -- ^ Child  node index
                      -> [Int]        -- ^ deletion events
                      -> [(Int, Int)] -- ^ Insertion events
                      -> Assertion
insertionDeletionTest rootRef symbols spec parentRef childRef expectedDeletions expectedInsertions = undefined
  where
    inputTree  = createSimpleTree rootRef symbols spec
    outputTree = allOptimization 1 defMeta inputTree
