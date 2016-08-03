-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.Test.Trees
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

module Analysis.ImpliedAlignment.Test.Trees where

import           Analysis.Parsimony.Binary.Internal
import           Analysis.ImpliedAlignment.DynamicProgramming
import           Bio.Metadata
import           Bio.PhyloGraph            hiding (name)
import           Data.Alphabet
import           Data.Vector             (Vector)
import           Test.Custom
import           Test.Tasty
import           Test.Tasty.HUnit

--import Debug.Trace

{-
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   SUPPORTING DATA-STRUCTURES AND FUNCTIONS
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-}

-- | A meta data structure with the cheap indel cost (1) and large substituion cost (4).
sillyMeta :: Vector (CharacterMetadata s)
sillyMeta =
  pure CharMeta
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

-- | A meta data structure with the typical indel cost (2) and substituion cost (1).
defaultMeta :: Vector (CharacterMetadata s)
defaultMeta =
  pure CharMeta
    { charType   = DirectOptimization
    , alphabet   = constructAlphabet []
    , name       = "DefaultCharacter"
    , isAligned  = False
    , isIgnored  = False
    , weight     = 1.0
    , stateNames = mempty
    , fitchMasks = undefined
    , rootCost   = 0.0
    , costs      = GeneralCost { indelCost = 2, subCost = 1 }
    }

-- | A meta data structure with the equal indel cost (1) and substituion cost (1).
equalMeta :: Vector (CharacterMetadata s)
equalMeta =
  pure CharMeta
    { charType   = DirectOptimization
    , alphabet   = constructAlphabet []
    , name       = "DefaultCharacter"
    , isAligned  = False
    , isIgnored  = False
    , weight     = 1.0
    , stateNames = mempty
    , fitchMasks = undefined
    , rootCost   = 0.0
    , costs      = GeneralCost { indelCost = 2, subCost = 1 }
    }


{- |
  A tree transformation that applies the direct optimization decoration followed
  by the implied alignment decoration to a tree.
-}
performImpliedAlignment :: SimpleTree -> SimpleTree
performImpliedAlignment = deriveImpliedAlignments meta . allOptimization 1 meta
  where
    meta = defaultMeta 

{- |
  Takes an adjacency list representation of a tree with annotations of inital
  characters and the resulting valid implied alignment character decorations.
-}
decorationTest :: Foldable t => t (Int, String, [String], [Int]) -> Assertion
decorationTest = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" performImpliedAlignment getHomologies'

{-
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   TEST TREE BRANCHES
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-}

-- | The entire set of test case trees
testImpliedAlignmentCases :: TestTree
testImpliedAlignmentCases = testGroup "Explicit test cases for implied alignment"
    [ testSimpleInsertionDeletionBiasing
    , testDeletedInsertions
    , testInsertedDeletions
    , testAdjacentDeletionInsertionEvents
    , testAdjacentDeletionInsertionEvents2
    , testTheDamnTrucnation
    ]

{- |
  A set of simple tests to ensure that a series of insertions or deletions
  behave as expected when applied in both directions.
-}
testSimpleInsertionDeletionBiasing :: TestTree
testSimpleInsertionDeletionBiasing = testGroup "Insertion & deletion event appending & prepending to character"
    [ testAppendedDeletions
    , testPrependedDeletions
    , testAppendedInsertions
    , testPrependedInsertions
    ]

testDeletedInsertions :: TestTree
testDeletedInsertions = testGroup "Deletion of insertion events"
    [ testDeletedInsertionSingle
    , testDeletedInsertionGroupMiddle
    , testDeletedInsertionGroupPrepend
    , testDeletedInsertionGroupAppend
    , testDeletedInsertionAntisymetry
    , testDoubleDeletedInsertion
    ]


testInsertedDeletions :: TestTree
testInsertedDeletions = testGroup "Insertion of deletion events"
    [ testInsertedDeletion
    , testInsertedDeletion2
    , testInsertedDeletion3
    ]

{-
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   TEST TREE CASES
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-}

{- |
  This tree should contain 3 deletion events which are applied repeatedly to the
  /end/ of the character as you traverse down the tree.

  0
  |
  +-1: ACGT
  |
  `-2
    |
    +-3: ACG
    |
    `-4
      |
      +-5: AC
      |
      `-6: A
-}
testAppendedDeletions :: TestTree
testAppendedDeletions = testCase "Chain of deletions appended to sequence" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    ], [ 1, 2])
           , ( 1, "ACGT", ["ACGT"], []     )
           , ( 2, ""    , [""    ], [ 3, 4])
           , ( 3, "ACG" , ["ACG-"], []     )
           , ( 4, ""    , [""    ], [ 5, 6])
           , ( 5, "AC"  , ["AC--"], []     )
           , ( 6, "A"   , ["A---"], []     )
           ]

{- |
  This tree should contain 3 deletion events which are applied repeatedly to the
  /beginning/ of the character as you traverse down the tree.

  0
  |
  +-1: TGCA
  |
  `-2
    |
    +-3: GCA
    |
    `-4
      |
      +-5: CA
      |
      `-6: A
-}
testPrependedDeletions :: TestTree
testPrependedDeletions = testCase "Chain of deletions prepended to sequence" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    ], [ 1, 2])
           , ( 1, "TGCA", ["TGCA"], []     )
           , ( 2, ""    , [""    ], [ 3, 4])
           , ( 3, "GCA" , ["-GCA"], []     )
           , ( 4, ""    , [""    ], [ 5, 6])
           , ( 5, "CA"  , ["--CA"], []     )
           , ( 6, "A"   , ["---A"], []     )
           ]
{- |
  This tree should contain 3 insertion events which are applied repeatedly to the
  /end/ of the character as you traverse down the tree.

  0
  |
  +-1: A
  |
  `-2
    |
    +-3: AC
    |
    `-4
      |
      +-5: ACG
      |
      `-6: ACGT
-}
testAppendedInsertions :: TestTree
testAppendedInsertions = testCase "Chain of insertions appended to sequence" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [    ""], [ 1, 2])
           , ( 1, "A"   , ["A---"], []     )
           , ( 2, ""    , [    ""], [ 3, 4])
           , ( 3, "AC"  , ["AC--"], []     )
           , ( 4, ""    , [    ""], [ 5, 6])
           , ( 5, "ACG" , ["ACG-"], []     )
           , ( 6, "ACGT", ["ACGT"], []     )
           ]

{- |
  This tree should contain 3 insertion events which are applied repeatedly to the
  /beginning/ of the character as you traverse down the tree.

  0
  |
  +-1: A
  |
  `-2
    |
    +-3: CA
    |
    `-4
      |
      +-5: GCA
      |
      `-6: TGCA
-}
testPrependedInsertions :: TestTree
testPrependedInsertions = testCase "Chain of insertions prepended to sequence" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    ], [ 1, 2])
           , ( 1, "A"   , ["---A"], []     )
           , ( 2, ""    , [""    ], [ 3, 4])
           , ( 3, "CA"  , ["--CA"], []     )
           , ( 4, ""    , [""    ], [ 5, 6])
           , ( 5, "GCA" , ["-GCA"], []     )
           , ( 6, "TGCA", ["TGCA"], []     )
           ]

testAdjacentDeletionInsertionEvents2 :: TestTree
testAdjacentDeletionInsertionEvents2 = testCase "Pair of adjacent insertion & deletion events (insertion should be first)" $ testHarness tree
  where
    testHarness    = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" transformation getHomologies'
    transformation = deriveImpliedAlignments sillyMeta . allOptimization 1 sillyMeta
    tree = [ ( 0, ""     , [""      , ""      , ""      , ""      ], [ 1, 2])
           , ( 1, "ACTAA", ["A-CTAA", "AC-TAA", "A-CTAA", "AC-TAA"], []     )
           , ( 2, ""     , [""      , ""      , ""      , ""      ], [ 3, 4])
           , ( 3, "ACTAA", ["A-CTAA", "AC-TAA", "A-CTAA", "AC-TAA"], []     )
           , ( 4, ""     , [""      , ""      , ""      , ""      ], [ 5, 6])
           , ( 5, "ATTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"], []     )
           , ( 6, "ATTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"], []     )
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


testDeletedInsertionAntisymetry :: TestTree
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

testDoubleDeletedInsertion :: TestTree
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

testInsertedDeletion :: TestTree
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

testInsertedDeletion2 :: TestTree
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


testInsertedDeletion3 :: TestTree
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

testDeletedInsertionSingle :: TestTree
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

testDeletedInsertionGroupMiddle :: TestTree
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

testDeletedInsertionGroupPrepend :: TestTree
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

testDeletedInsertionGroupAppend :: TestTree
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

testTheDamnTrucnation :: TestTree
testTheDamnTrucnation = testCase "That god damn truncation issue" $ decorationTest tree
  where
    tree = [ ( 0, ""      , [""     , ""     ], [ 1, 2])
           , ( 1, "AT"    , ["A---T", "A---T"], []     )
           , ( 2, ""      , [""     , ""     ], [ 3, 4])
           , ( 3, "AT"    , ["A---T", "A---T"], []     )
           , ( 4, ""      , [""     , ""     ], [ 5, 6])
           , ( 5, "ACT"   , ["AC--T", "A--CT"], []     )
           , ( 6, ""      , [""     , ""     ], [ 7, 8])
           , ( 7, "ACT"   , ["AC--T", "A--CT"], []     )
           , ( 8, ""      , [""     , ""     ], [ 9,10])
           , ( 9, "ACCT"  , ["ACC-T", "A-CCT"], []     )
           , (10, "ACCCT" , ["ACCCT", "ACCCT"], []     )
           ]
