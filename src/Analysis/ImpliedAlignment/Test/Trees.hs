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


-- | A metadata structure with a cheap indel cost (1) and large substitution cost (4).
sillyMeta :: Vector (CharacterMetadata s)
sillyMeta =
  pure CharMeta
    { charType   = DirectOptimization
    , alphabet   = undefined
    , name       = "DefaultCharacter"
    , isAligned  = False
    , isIgnored  = False
    , weight     = 1.0
    , stateNames = mempty
    , fitchMasks = undefined
    , rootCost   = 0.0
    , costs      = GeneralCost { indelCost = 1, subCost = 4 }
    }


-- | A metadata structure with the typical indel cost (2) and substitution cost (1).
defaultMeta :: Vector (CharacterMetadata s)
defaultMeta =
  pure CharMeta
    { charType   = DirectOptimization
    , alphabet   = undefined
    , name       = "DefaultCharacter"
    , isAligned  = False
    , isIgnored  = False
    , weight     = 1.0
    , stateNames = mempty
    , fitchMasks = undefined
    , rootCost   = 0.0
    , costs      = GeneralCost { indelCost = 2, subCost = 1 }
    }


-- | A metadata structure with equal indel and substitution costs (1).
equalMeta :: Vector (CharacterMetadata s)
equalMeta =
  pure CharMeta
    { charType   = DirectOptimization
    , alphabet   = undefined
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
  Takes an adjacency list representation of a tree with annotations of initial
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
    , testNonHomology
    , testAdjacentDeletionInsertionEvents
    , testAdjacentDeletionInsertionEvents2
    , testTheDamnTrucnation
    , testDeletionInsertionInterrelations
    , testNestedInsertions
    , testBranchesWithAdjacentInsertions
    , testAmbigousResolutionConsistency
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


{- |
  A set of test trees which will later have their insertion events deleted. These events
  should /not/ be non-homology events.
-}
testDeletedInsertions :: TestTree
testDeletedInsertions = testGroup "Deletion of insertion events (no non-homology events)"
    [ testDeletedInsertionSingle
    , testDeletedInsertionGroupMiddle
    , testDeletedInsertionGroupPrepend
    , testDeletedInsertionGroupAppend
    , testTwoAdjacentInsertionsSimultaneousDeletions
    , testTwoNonAdjacentInsertionsSimultaneousDeletions
    , testTwoAdjacentInsertionsSeperateDeletions
    , testTwoNonAdjacentSymetricInsertionsSeperateDeletions
    , testTwoNonAdjacentAntiSymetricInsertionsSeperateDeletions
    ]


{- |
  A set of test trees which will later have their deletion events filled with an insertion
  event. These events should /not/ be non-homology events.
-}
testInsertedDeletions :: TestTree
testInsertedDeletions = testGroup "Insertion of deletion events (non-homology events by definition)"
    [ testInsertedDeletion
    , testInsertedDeletion3
    ]


{- |
  A set of test trees which invoke non-homology through insertion and/or
  deletion events.
-}
testNonHomology :: TestTree
testNonHomology = testGroup "Non-homology events from insertion and/or deletion events"
    [ testNonHomologyDualInsertions
    , testNonHomologyDualInsertionsWithExtraBase
    , testNonHomologyDeletedInsertion
    , testNonHomologyDoubleDeletedInsertion
    , testAmbiguousNonHomologyInsertions
    ]


{- |
  A set of test trees which tests the indexing between insertion and deletion
  events occurring at various places in the tree and character.
-}
testDeletionInsertionInterrelations :: TestTree
testDeletionInsertionInterrelations = testGroup "Correct relative indexing between insertion and deletion events"
    [ testDeletionBeforeAboveInsertion
    , testDeletionBeforeBelowInsertion
    , testDeletionAfterAboveInsertion
    , testDeletionAfterBelowInsertion
    ]


{-
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   TEST TREE CASES
 -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-}


{- |
  This tree should contain three deletion events that are applied repeatedly to the
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
  This tree should contain three deletion events that are applied repeatedly to the
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
  This tree should contain three insertion events that are applied repeatedly to the
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
  This tree should contain three insertion events that are applied repeatedly to the
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


{- |
  This tree should contain an insertion event and a deletion event that are
  adjacent. We need to make sure that these events are calculated correctly.

  It should be noted that we use a unique cost structure to force the adjacency
  on such a small tree.

  0
  |
  +-1: ACA
  |
  `-2
    |
    +-3: ACA
    |
    `-4
      |
      +-5: ATA
      |
      `-6: ATA
-}
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


{- |
  This tree should contain an insertion event and a deletion event that are
  adjacent. We need to make sure that these events are calculated correctly.

  It should be noted that we use a unique cost structure to force the adjacency
  on such a small tree.

  The alignment of trailing AA is not the focus of this test case.

  0
  |
  +-1: ACTAA
  |
  `-2
    |
    +-3: ACTAA
    |
    `-4
      |
      +-5: ATTA
      |
      `-6: ATTA
-}
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


{- |
  This tree should contain two insertion events on each branch between the
  AAA and ACATA characters, followed by a deletion event between the
  AAA and AATA/ACAA characters.

  We should note that both insertion events of C & T on each side of the root
  note between the AAA and ACATA characters are /non-homologous/ and should
  result in anti-symmetric gaps in the leaf node alignments.

  0
  |
  +-1
  | |
  | +-16: AAA
  | |
  | `-3
  |   |
  |   +-15: AAA
  |   |
  |   `-5
  |     |
  |     +-14: ACATA
  |     |
  |     `-7
  |       |
  |       +-13: ACATA
  |       |
  |       `-9
  |         |
  |         +-11: ACAA
  |         |
  |         `-12: ACAA
  |
  `-2
    |
    +-17: AAA
    |
    `-4
      |
      +-18: AAA
      |
      `-6
        |
        +-19: ACATA
        |
        `-8
          |
          +-20: ACATA
          |
          `-10
            |
            +-21: AATA
            |
            `-22: AATA

-}
testNonHomologyDeletedInsertion :: TestTree
testNonHomologyDeletedInsertion = testCase "Deleted insertion events anti-symetrically reflected across the root" $ decorationTest tree
  where
    tree = [ ( 0, ""     , [""       , ""       , ""       , ""       ], [ 1, 2])
           , ( 1, ""     , [""       , ""       , ""       , ""       ], [ 3,16])
           , ( 2, ""     , [""       , ""       , ""       , ""       ], [ 4,17])
           , ( 3, ""     , [""       , ""       , ""       , ""       ], [ 5,15])
           , ( 4, ""     , [""       , ""       , ""       , ""       ], [ 6,18])
           , ( 5, ""     , [""       , ""       , ""       , ""       ], [ 7,14])
           , ( 6, ""     , [""       , ""       , ""       , ""       ], [ 8,19])
           , ( 7, ""     , [""       , ""       , ""       , ""       ], [ 9,13])
           , ( 8, ""     , [""       , ""       , ""       , ""       ], [10,20])
           , ( 9, ""     , [""       , ""       , ""       , ""       ], [11,12])
           , (10, ""     , [""       , ""       , ""       , ""       ], [21,22])
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


{- |
  This tree should contain an three insertion events. These insertion events
  should have /consecutive/ indices. All the insertion events are /non-homologous/.

  0
  |
  +-1
  | |
  | +-7: AA
  | |
  | `-8
  |   |
  |   +-9
  |   | |
  |   | +-11: AA
  |   | |
  |   | `-12: AGA
  |   |
  |   `-10:
  |     |
  |     +-13: AA
  |     |
  |     `-14: ATA
  |
  `-2
    |
    +-3: AA
    |
    `-4
      |
      +-5: AA
      |
      `-6: ACA
-}
testAmbiguousNonHomologyInsertions :: TestTree
testAmbiguousNonHomologyInsertions = testCase "multiple ambigously placed non-homology insertion events" $ decorationTest tree
  where
    tree = [ ( 0, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [ 1, 2])
           , ( 1, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [ 7, 8])
           , ( 2, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [ 3, 4])
           , ( 3, "AA" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"], []     )
           , ( 4, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [ 5, 6])
           , ( 5, "AA" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"], []     )
           , ( 6, "ACA", ["AC--A", "AC--A", "A-C-A", "A--CA", "A-C-A", "A--CA"], []     )
           , ( 7, "AA" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"], []     )
           , ( 8, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [ 9,10])
           , ( 9, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [11,12])
           , (10, ""   , [""     , ""     , ""     , ""     , ""     , ""     ], [13,14])
           , (11, "AA" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"], []     )
           , (12, "AGA", ["A-G-A", "A--GA", "AG--A", "AG--A", "A--GA", "A-G-A"], []     )
           , (13, "AA" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"], []     )
           , (14, "ATA", ["A--TA", "A-T-A", "A--TA", "A-T-A", "AT--A", "AT--A"], []     )
           ]


{- |
  This tree should contain one deletion event followed by an insertion event.

  We should note that the deletion event and subsequent insertion event of the
  T base are /non-homologous/!

  The presence of an additional T base in all the leaf characters tests the
  appropriate handling of the base ambiguity by the implied alignment.

  0
  |
  +-1: ATT
  |
  `-2
    |
    +-3: ATT
    |
    `-4
      |
      +-5: AT
      |
      `-6
        |
        +-7: AT
        |
        `-8
          |
          +-9: AT
          |
          `-10
            |
            +-11: ATT
            |
            `-12: ATT
-}
testInsertedDeletion :: TestTree
testInsertedDeletion = testCase "Insertion event of an deletion event" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    , ""    , ""    , ""    , ""    , ""    ], [ 1, 2])
           , ( 1, "ATT" , ["AT-T", "ATT-", "A-TT", "ATT-", "A-TT", "AT-T"], []     )
           , ( 2, ""    , [""    , ""    , ""    , ""    , ""    , ""    ], [ 3, 4])
           , ( 3, "ATT" , ["AT-T", "ATT-", "A-TT", "ATT-", "A-TT", "AT-T"], []     )
           , ( 4, ""    , [""    , ""    , ""    , ""    , ""    , ""    ], [ 5, 6])
           , ( 5, "AT"  , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"], []     )
           , ( 6, ""    , [""    , ""    , ""    , ""    , ""    , ""    ] , [ 7, 8])
           , ( 7, "AT"  , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"], []     )
           , ( 8, ""    , [""    , ""    , ""    , ""    , ""    , ""    ], [ 9,10])
           , ( 9, "AT"  , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"], []     )
           , (10, ""    , [""    , ""    , ""    , ""    , ""    , ""    ], [11,12])
           , (11, "ATT" , ["A-TT", "A-TT", "AT-T", "AT-T", "ATT-", "ATT-"], []     )
           , (12, "ATT" , ["A-TT", "A-TT", "AT-T", "AT-T", "ATT-", "ATT-"], []     )
           ]

{- |
  This tree should contain one deletion event on rhe edge between node 5 and
  node 7. Thre should also be an insertion event on the edge between node 11
  and node 13.

  We should note that the insertion event is /non-homologous/ with the similar
  base earlier in the tree!

  The presence of an additional T base in all the leaf characters tests the
  appropriate handling of the base ambiguity by the implied alignment.

  0
  |
  +-1
  | |
  | +-2: ATTA
  | |
  | `-3: ATTA
  |
  `-4
    |
    +-5: ATTA
    |
    `-6
      |
      +-7: ATA
      |
      `-8
        |
        +-9: ATA
        |
        `-10:
          |
          +-11: ATA
          |
          `-12:
            |
            +-13: ATTA
            |
            `-14: ATTA
-}
testInsertedDeletion3 :: TestTree
testInsertedDeletion3 = testCase "Insertion event of an deletion event 3" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [ 1, 4])
           , ( 1, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [ 2, 3])
           , ( 2, "ATTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"], []     )
           , ( 3, "ATTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"], []     )
           , ( 4, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [ 5, 6])
           , ( 5, "ATTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"], []     )
           , ( 6, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [ 7, 8])
           , ( 7, "ATA" , ["A--TA", "A-T-A", "A-TTA", "AT--A", "A-T-A", "AT--A"], []     )
           , ( 8, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [ 9,10])
           , ( 9, "ATA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , (10, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [11,12])
           , (11, "ATA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , (12, ""    , [""     , ""     , ""     , ""     , ""     , ""     ], [13,14])
           , (13, "ATTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"], []     )
           , (14, "ATTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"], []     )
           ]


{- |
  This tree should contain one insertion event on each branch between the
  AA and ATA characters.

  We should note that these insertion events of the T base on each side of the
  root note between the AA and ATA characters are /non-homologous/ and should
  result in anti-symmetric gaps in the leaf node alignments.

  0
  |
  +-2
  | |
  | +-3: AA
  | |
  | `-4
  |   |
  |   +-5: AA
  |   |
  |   `-6
  |     |
  |     +-7: ATA
  |     |
  |     `-8: ATA
  |
  `-1
    |
    +-9: AA
    |
    `-10
      |
      +-11: AA
      |
      `-12
        |
        +-13: ATA
        |
        `-14: ATA
-}
testNonHomologyDualInsertions :: TestTree
testNonHomologyDualInsertions = testCase "Non-homology dual insertions of base" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    , ""    ], [ 1, 2])
           , ( 1, ""    , [""    , ""    ], [ 9,10])
           , ( 2, ""    , [""    , ""    ], [ 3, 4])
           , ( 3, "AA"  , ["A--A", "A--A"], []     )
           , ( 4, ""    , [""    , ""    ], [ 5, 6])
           , ( 5, "AA"  , ["A--A", "A--A"], []     )
           , ( 6, ""    , [""    , ""    ], [ 7, 8])
           , ( 7, "ATA" , ["AT-A", "A-TA"], []     )
           , ( 8, "ATA" , ["AT-A", "A-TA"], []     )
           , ( 9, "AA"  , ["A--A", "A--A"], []     )
           , (10, ""    , [""    , ""    ], [11,12])
           , (11, "AA"  , ["A--A", "A--A"], []     )
           , (12, ""    , [""    , ""    ], [13,14])
           , (13, "ATA" , ["A-TA", "AT-A"], []     )
           , (14, "ATA" , ["A-TA", "AT-A"], []     )
           ]

{- |
  This tree should contain an insertion event on the edge between node 3 and
  node 5. This insertion event should be deleted on the edge between node 9
  and node 11. This tree should also contain a second insertion event on the
  edge between node 14 and 16.

  We should note that both insertion events of T are /non-homologous/!

  0
  |
  +-1: AA
  |
  `-2
    |
    +-3: AA
    |
    `-4
      |
      +-5: ATA
      |
      `-6
        |
        +-7: ATA
        |
        `-8
          |
          +-9: ATA
          |
          `-10
            |
            +-11: AA
            |
            `-12
              |
              +-13: AA
              |
              `-14
                |
                +-15: AA
                |
                `-16
                  |
                  +-17: ATA
                  |
                  `-18: ATA
-}
testNonHomologyDoubleDeletedInsertion :: TestTree
testNonHomologyDoubleDeletedInsertion = testCase "Double deletion event of an single insertion event" $ decorationTest tree
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


{- |
  This tree should contain one insertion event on each branch between the
  ATA and ATTA characters.

  We should note that these insertion events of the T base on each side of the
  root note between the ATA and ATTA characters are /non-homologous/ and should
  result in anti-symmetric gaps in the leaf node alignments.

  The presence of an additional T base in all the leaf characters tests the
  appropriate handling of the base ambiguity by the implied alignment.

  0
  |
  +-2
  | |
  | +-3: ATA
  | |
  | `-4
  |   |
  |   +-5: ATA
  |   |
  |   `-6
  |     |
  |     +-7: ATTA
  |     |
  |     `-8: ATTA
  |
  `-1
    |
    +-9: ATA
    |
    `-10
      |
      +-11: ATA
      |
      `-12
        |
        +-13: ATTA
        |
        `-14: ATTA
-}
testNonHomologyDualInsertionsWithExtraBase :: TestTree
testNonHomologyDualInsertionsWithExtraBase = testCase "Non-homology dual insertions of base with lingering identical base" $ decorationTest tree
  where
    tree = [ ( 0, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [ 1, 2])
           , ( 1, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [ 9,10])
           , ( 2, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [ 3, 4])
           , ( 3, "ATA"  , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , ( 4, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [ 5, 6])
           , ( 5, "ATA"  , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , ( 6, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [ 7, 8])
           , ( 7, "ATTA" , ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"], []     )
           , ( 8, "ATTA" , ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"], []     )
           , ( 9, "ATA"  , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , (10, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [11,12])
           , (11, "ATA"  , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"], []     )
           , (12, ""     , [""     , ""     , ""     , ""     , ""     , ""     ], [13,14])
           , (13, "ATTA" , ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"], []     )
           , (14, "ATTA" , ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"], []     )
           ]


{- |
  This tree should contain one insertion event and one deletion event. The
  deletion event should be applied to the same index as the inserted base.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AACTT
        |
        `-8
          |
          +-9: AACTT
          |
          `-10
            |
            +-11: AACTT
            |
            `-12
              |
              +-13: AATT
              |
              `-14: AATT
-}
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


{- |
  This tree should contain three /consecutive/ insertion events and one deletion
  event. The deletion event should be applied to the /middle/ index of the
  inserted bases.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AACGCTT
        |
        `-8
          |
          +-9: AACGCTT
          |
          `-10
            |
            +-11: AACGCTT
            |
            `-12
              |
              +-13: AACCTT
              |
              `-14: AACCTT
-}
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


{- |
  This tree should contain three /consecutive/ insertion events and one deletion
  event. The deletion event should be applied to the /first/ index of the
  inserted bases.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AAGCCTT
        |
        `-8
          |
          +-9: AAGCCTT
          |
          `-10
            |
            +-11: AAGCCTT
            |
            `-12
              |
              +-13: AACCTT
              |
              `-14: AACCTT
-}
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


{- |
  This tree should contain three /consecutive/ insertion events and one deletion
  event. The deletion event should be applied to the /last/ index of the
  inserted bases.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AACCGTT
        |
        `-8
          |
          +-9: AACCGTT
          |
          `-10
            |
            +-11: AACCGTT
            |
            `-12
              |
              +-13: AACCTT
              |
              `-14: AACCTT
-}
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


{- |
  This tree should contain two /adjacent/ insertion events on different edges
  and two deletion events on a single edge.

  The deletion events should be applied to the same indices as the inserted
  bases.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AACTT
        |
        `-8
          |
          +-9: AACTT
          |
          `-10
            |
            +-11: AACTT
            |
            `-12
              |
              +-13: AACCTT
              |
              `-14
                 |
                 +-15: AACCTT
                 |
                 `-16
                   |
                   +-17: AACCTT
                     |
                     `-18
                       |
                       +-19: AATT
                       |
                       `-20: AATT
-}
testTwoAdjacentInsertionsSimultaneousDeletions :: TestTree
testTwoAdjacentInsertionsSimultaneousDeletions = testCase "Delete two     adjacent insertion events on a single edge" $ decorationTest tree
  where
    tree = [ ( 0, ""      , [""      , ""      ], [ 1, 2])
           , ( 1, "AATT"  , ["AA--TT", "AA--TT"], []     )
           , ( 2, ""      , [""      , ""      ], [ 3, 4])
           , ( 3, "AATT"  , ["AA--TT", "AA--TT"], []     )
           , ( 4, ""      , [""      , ""      ], [ 5, 6])
           , ( 5, "AATT"  , ["AA--TT", "AA--TT"], []     )
           , ( 6, ""      , [""      , ""      ], [ 7, 8])
           , ( 7, "AACTT" , ["AA-CTT", "AAC-TT"], []     )
           , ( 8, ""      , [""      , ""      ], [ 9,10])
           , ( 9, "AACTT" , ["AA-CTT", "AAC-TT"], []     )
           , (10, ""      , [""      , ""      ], [11,12])
           , (11, "AACTT" , ["AA-CTT", "AAC-TT"], []     )
           , (12, ""      , [""      , ""      ], [13,14])
           , (13, "AACCTT", ["AACCTT", "AACCTT"], []     )
           , (14, ""      , [""      , ""      ], [15,16])
           , (15, "AACCTT", ["AACCTT", "AACCTT"], []     )
           , (16, ""      , [""      , ""      ], [17,18])
           , (17, "AACCTT", ["AACCTT", "AACCTT"], []     )
           , (18, ""      , [""      , ""      ], [19,20])
           , (19, "AATT"  , ["AA--TT", "AA--TT"], []     )
           , (20, "AATT"  , ["AA--TT", "AA--TT"], []     )
           ]


{- |
  This tree should contain two /non-adjacdent/ insertion events on different
  edges and two deletion events on a single edge.

  The deletion events should be applied to the same indices as the inserted
  bases.

  0
  |
  +-1: AAGTT
  |
  `-2
    |
    +-3: AAGTT
    |
    `-4
      |
      +-5: AAGTT
      |
      `-6
        |
        +-7: AACGTT
        |
        `-8
          |
          +-9: AACGTT
          |
          `-10
            |
            +-11: AACGTT
            |
            `-12
              |
              +-13: AACGCTT
              |
              `-14
                 |
                 +-15: AACGCTT
                 |
                 `-16
                   |
                   +-17: AACGCTT
                     |
                     `-18
                       |
                       +-19: AATT
                       |
                       `-20: AATT
-}
testTwoNonAdjacentInsertionsSimultaneousDeletions :: TestTree
testTwoNonAdjacentInsertionsSimultaneousDeletions = testCase "Delete two non-adjacent insertion events on a single edge" $ decorationTest tree
  where
    tree = [ ( 0, ""       , [""       ], [ 1, 2])
           , ( 1, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 2, ""       , [""       ], [ 3, 4])
           , ( 3, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 4, ""       , [""       ], [ 5, 6])
           , ( 5, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 6, ""       , [""       ], [ 7, 8])
           , ( 7, "AACGTT" , ["AACG-TT"], []     )
           , ( 8, ""       , [""       ], [ 9,10])
           , ( 9, "AACGTT" , ["AACG-TT"], []     )
           , (10, ""       , [""       ], [11,12])
           , (11, "AACGTT" , ["AACG-TT"], []     )
           , (12, ""       , [""       ], [13,14])
           , (13, "AACGCTT", ["AACGCTT"], []     )
           , (14, ""       , [""       ], [15,16])
           , (15, "AACGCTT", ["AACGCTT"], []     )
           , (16, ""       , [""       ], [17,18])
           , (17, "AACGCTT", ["AACGCTT"], []     )
           , (18, ""       , [""       ], [19,20])
           , (19, "AAGTT"  , ["AA-G-TT"], []     )
           , (20, "AAGTT"  , ["AA-G-TT"], []     )
           ]


{- |
  This tree should contain two /adjacent/ insertion events on different edges
  and two deletion events on different edges.

  The deletion events should be applied to the same indices as the inserted
  bases.

  0
  |
  +-1: AATT
  |
  `-2
    |
    +-3: AATT
    |
    `-4
      |
      +-5: AATT
      |
      `-6
        |
        +-7: AACTT
        |
        `-8
          |
          +-9: AACTT
          |
          `-10
            |
            +-11: AACTT
            |
            `-12
              |
              +-13: AACCTT
              |
              `-14
                 |
                 +-15: AACCTT
                 |
                 `-16
                   |
                   +-17: AACCTT
                   |
                   `-18
                     |
                     +-19: AACTT
                     |
                     `-20
                       |
                       +-21: AACTT
                       |
                       `-22
                          |
                          +-23: AACTT
                          |
                          `-24
                            |
                            +-25: AATT
                            |
                            `-26: AATT
-}
testTwoAdjacentInsertionsSeperateDeletions :: TestTree
testTwoAdjacentInsertionsSeperateDeletions = testCase "Delete two     adjacent insertion events on different edges" $ decorationTest tree
  where
    tree = [ ( 0, ""      , [""      , ""      , ""      , ""      ], [ 1, 2])
           , ( 1, "AATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"], []     )
           , ( 2, ""      , [""      , ""      , ""      , ""      ], [ 3, 4])
           , ( 3, "AATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"], []     )
           , ( 4, ""      , [""      , ""      , ""      , ""      ], [ 5, 6])
           , ( 5, "AATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"], []     )
           , ( 6, ""      , [""      , ""      , ""      , ""      ], [ 7, 8])
           , ( 7, "AACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"], []     )
           , ( 8, ""      , [""      , ""      , ""      , ""      ], [ 9,10])
           , ( 9, "AACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"], []     )
           , (10, ""      , [""      , ""      , ""      , ""      ], [11,12])
           , (11, "AACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"], []     )
           , (12, ""      , [""      , ""      , ""      , ""      ], [13,14])
           , (13, "AACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"], []     )
           , (14, ""      , [""      , ""      , ""      , ""      ], [15,16])
           , (15, "AACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"], []     )
           , (16, ""      , [""      , ""      , ""      , ""      ], [17,18])
           , (17, "AACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"], []     )
           , (18, ""      , [""      , ""      , ""      , ""      ], [19,20])
           , (19, "AACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"], []     )
           , (20, ""      , [""      , ""      , ""      , ""      ], [21,22])
           , (21, "AACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"], []     )
           , (22, ""      , [""      , ""      , ""      , ""      ], [23,24])
           , (23, "AACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"], []     )
           , (24, ""      , [""      , ""      , ""      , ""      ], [25,26])
           , (25, "AATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"], []     )
           , (26, "AATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"], []     )
           ]


{- |
  This tree should contain two /non-adjacent/ insertion events on different edges
  and two deletion events on different edges. The insertion events follow a
  symmetric pattern.

  The deletion events should be applied to the same indices as the inserted
  bases.

  0
  |
  +-1: AAGTT
  |
  `-2
    |
    +-3: AAGTT
    |
    `-4
      |
      +-5: AAGTT
      |
      `-6
        |
        +-7: AACGTT
        |
        `-8
          |
          +-9: AACGTT
          |
          `-10
            |
            +-11: AACGTT
            |
            `-12
              |
              +-13: AACGCTT
              |
              `-14
                 |
                 +-15: AACGCTT
                 |
                 `-16
                   |
                   +-17: AACGCTT
                   |
                   `-18
                     |
                     +-19: AACGTT
                     |
                     `-20
                       |
                       +-21: AACGTT
                       |
                       `-22
                          |
                          +-23: AACGTT
                          |
                          `-24
                            |
                            +-25: AAGTT
                            |
                            `-26: AAGTT
-}
testTwoNonAdjacentSymetricInsertionsSeperateDeletions :: TestTree
testTwoNonAdjacentSymetricInsertionsSeperateDeletions = testCase "Delete two non-adjacent insertion events on different edges (symetric)" $ decorationTest tree
  where
    tree = [ ( 0, ""       , [""       ], [ 1, 2])
           , ( 1, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 2, ""       , [""       ], [ 3, 4])
           , ( 3, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 4, ""       , [""       ], [ 5, 6])
           , ( 5, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 6, ""       , [""       ], [ 7, 8])
           , ( 7, "AACGTT" , ["AACG-TT"], []     )
           , ( 8, ""       , [""       ], [ 9,10])
           , ( 9, "AACGTT" , ["AACG-TT"], []     )
           , (10, ""       , [""       ], [11,12])
           , (11, "AACGTT" , ["AACG-TT"], []     )
           , (12, ""       , [""       ], [13,14])
           , (13, "AACGCTT", ["AACGCTT"], []     )
           , (14, ""       , [""       ], [15,16])
           , (15, "AACGCTT", ["AACGCTT"], []     )
           , (16, ""       , [""       ], [17,18])
           , (17, "AACGCTT", ["AACGCTT"], []     )
           , (18, ""       , [""       ], [19,20])
           , (19, "AACGTT" , ["AACG-TT"], []     )
           , (20, ""       , [""       ], [21,22])
           , (21, "AACGTT" , ["AACG-TT"], []     )
           , (22, ""       , [""       ], [23,24])
           , (23, "AACGTT" , ["AACG-TT"], []     )
           , (24, ""       , [""       ], [25,26])
           , (25, "AAGTT"  , ["AA-G-TT"], []     )
           , (26, "AAGTT"  , ["AA-G-TT"], []     )
           ]


{- |
  This tree should contain two /non-adjacent/ insertion events on different edges
  and two deletion events on different edges. The insertion events follow a
  symmetric pattern.

  The deletion events should be applied to the same indices as the inserted
  bases.

  0
  |
  +-1: AAGTT
  |
  `-2
    |
    +-3: AAGTT
    |
    `-4
      |
      +-5: AAGTT
      |
      `-6
        |
        +-7: AACGTT
        |
        `-8
          |
          +-9: AACGTT
          |
          `-10
            |
            +-11: AACGTT
            |
            `-12
              |
              +-13: AACGCTT
              |
              `-14
                 |
                 +-15: AACGCTT
                 |
                 `-16
                   |
                   +-17: AACGCTT
                   |
                   `-18
                     |
                     +-19: AAGCTT
                     |
                     `-20
                       |
                       +-21: AAGCTT
                       |
                       `-22
                          |
                          +-23: AAGCTT
                          |
                          `-24
                            |
                            +-25: AAGTT
                            |
                            `-26: AAGTT
-}
testTwoNonAdjacentAntiSymetricInsertionsSeperateDeletions :: TestTree
testTwoNonAdjacentAntiSymetricInsertionsSeperateDeletions = testCase "Delete two non-adjacent insertion events on different edges (antisymetric)" $ decorationTest tree
  where
    tree = [ ( 0, ""       , [""       ], [ 1, 2])
           , ( 1, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 2, ""       , [""       ], [ 3, 4])
           , ( 3, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 4, ""       , [""       ], [ 5, 6])
           , ( 5, "AAGTT"  , ["AA-G-TT"], []     )
           , ( 6, ""       , [""       ], [ 7, 8])
           , ( 7, "AACGTT" , ["AACG-TT"], []     )
           , ( 8, ""       , [""       ], [ 9,10])
           , ( 9, "AACGTT" , ["AACG-TT"], []     )
           , (10, ""       , [""       ], [11,12])
           , (11, "AACGTT" , ["AACG-TT"], []     )
           , (12, ""       , [""       ], [13,14])
           , (13, "AACGCTT", ["AACGCTT"], []     )
           , (14, ""       , [""       ], [15,16])
           , (15, "AACGCTT", ["AACGCTT"], []     )
           , (16, ""       , [""       ], [17,18])
           , (17, "AACGCTT", ["AACGCTT"], []     )
           , (18, ""       , [""       ], [19,20])
           , (19, "AAGCTT" , ["AA-GCTT"], []     )
           , (20, ""       , [""       ], [21,22])
           , (21, "AAGCTT" , ["AA-GCTT"], []     )
           , (22, ""       , [""       ], [23,24])
           , (23, "AAGCTT" , ["AA-GCTT"], []     )
           , (24, ""       , [""       ], [25,26])
           , (25, "AAGTT"  , ["AA-G-TT"], []     )
           , (26, "AAGTT"  , ["AA-G-TT"], []     )
           ]


{- |
  This tree should contain three insertion events.

  We should note that the two furthest leaf nodes contain a different number
  of insertion events. This induces a truncation issue, because the insertion
  events would not be applied correctly on the pre-order pass.

  0
  |
  +-1: AT
  |
  `-2
    |
    +-3: AT
    |
    `-4
      |
      +-5: ACT
      |
      `-6
        |
        +-7: ACT
        |
        `-8
          |
          +-9: ACCT
          |
          `-10: ACCCT
-}
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


{- |
  This tree should contain one insertion event and one deletion event.

  This tree tests the handling of an insertion event with a deletion event
  before it in the character and above it in the tree.

  0
  |
  +-1: GAA
  |
  `-2
    |
    +-3: GAA
    |
    `-4
      |
      +-5: AA
      |
      `-6
        |
        +-7: AA
        |
        `-8
          |
          +-9: AA
          |
          `-10: ACA
-}
testDeletionBeforeAboveInsertion :: TestTree
testDeletionBeforeAboveInsertion = testCase "Insertion event with deletion event before in character & above in tree" $ decorationTest tree
  where
    tree = [ ( 0, ""   , [""    ], [ 1, 2])
           , ( 1, "GAA", ["GA-A"], []     )
           , ( 2, ""   , [""    ], [ 3, 4])
           , ( 3, "GAA", ["GA-A"], []     )
           , ( 4, ""   , [""    ], [ 5, 6])
           , ( 5, "AA" , ["-A-A"], []     )
           , ( 6, ""   , [""    ], [ 7, 8])
           , ( 7, "AA" , ["-A-A"], []     )
           , ( 8, ""   , [""    ], [ 9,10])
           , ( 9, "AA" , ["-A-A"], []     )
           , (10, "ACA", ["-ACA"], []     )
           ]


{- |
  This tree should contain one insertion event and one deletion event.

  This tree tests the handling of an insertion event with a deletion event
  before it in the character and below it in the tree.

  0
  |
  +-1: GAA
  |
  `-2
    |
    +-3: GAA
    |
    `-4
      |
      +-5: GACA
      |
      `-6
        |
        +-7: GACA
        |
        `-8
          |
          +-9: GACA
          |
          `-10: ACA
-}
testDeletionBeforeBelowInsertion :: TestTree
testDeletionBeforeBelowInsertion = testCase "Insertion event with deletion event before in character & below in tree" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    ], [ 1, 2])
           , ( 1, "GAA" , ["GA-A"], []     )
           , ( 2, ""    , [""    ], [ 3, 4])
           , ( 3, "GAA" , ["GA-A"], []     )
           , ( 4, ""    , [""    ], [ 5, 6])
           , ( 5, "GACA", ["GACA"], []     )
           , ( 6, ""    , [""    ], [ 7, 8])
           , ( 7, "GACA", ["GACA"], []     )
           , ( 8, ""    , [""    ], [ 9,10])
           , ( 9, "GACA", ["GACA"], []     )
           , (10, "ACA" , ["-ACA"], []     )
           ]


{- |
  This tree should contain one insertion event and one deletion event.

  This tree tests the handling of an insertion event with a deletion event
  after it in the character and above it in the tree.

  0
  |
  +-1: AAG
  |
  `-2
    |
    +-3: AAG
    |
    `-4
      |
      +-5: AA
      |
      `-6
        |
        +-7: AA
        |
        `-8
          |
          +-9: AA
          |
          `-10: ACA
-}
testDeletionAfterAboveInsertion :: TestTree
testDeletionAfterAboveInsertion = testCase "Insertion event with deletion event  after in character & above in tree" $ decorationTest tree
  where
    tree = [ ( 0, ""   , [""    ], [ 1, 2])
           , ( 1, "AAG", ["A-AG"], []     )
           , ( 2, ""   , [""    ], [ 3, 4])
           , ( 3, "AAG", ["A-AG"], []     )
           , ( 4, ""   , [""    ], [ 5, 6])
           , ( 5, "AA" , ["A-A-"], []     )
           , ( 6, ""   , [""    ], [ 7, 8])
           , ( 7, "AA" , ["A-A-"], []     )
           , ( 8, ""   , [""    ], [ 9,10])
           , ( 9, "AA" , ["A-A-"], []     )
           , (10, "ACA", ["ACA-"], []     )
           ]


{- |
  This tree tests the handling of an insertion event with a deletion event
  after it in the character and below it in the tree.

  0
  |
  +-1: AAG
  |
  `-2
    |
    +-3: AAG
    |
    `-4
      |
      +-5: ACAG
      |
      `-6
        |
        +-7: ACAG
        |
        `-8
          |
           +-9: ACAG
          |
          `-10: ACA
-}
testDeletionAfterBelowInsertion :: TestTree
testDeletionAfterBelowInsertion = testCase "Insertion event with deletion event  after in character & below in tree" $ decorationTest tree
  where
    tree = [ ( 0, ""    , [""    ], [ 1, 2])
           , ( 1, "AAG" , ["A-AG"], []     )
           , ( 2, ""    , [""    ], [ 3, 4])
           , ( 3, "AAG" , ["A-AG"], []     )
           , ( 4, ""    , [""    ], [ 5, 6])
           , ( 5, "ACAG", ["ACAG"], []     )
           , ( 6, ""    , [""    ], [ 7, 8])
           , ( 7, "ACAG", ["ACAG"], []     )
           , ( 8, ""    , [""    ], [ 9,10])
           , ( 9, "ACAG", ["ACAG"], []     )
           , (10, "ACA" , ["ACA-"], []     )
           ]


{- |
  This tree contains an insertion event embedded in an earlier ancertral insertion sequence.

  This won't be handled appropriately with greedy pre-order traversal insertions unless either extra
  information is stored in the insertion event structure or pains are taken to perform linear
  probing during the folds.

  0
  |
  +-1: AA
  |
  `-2
    |
    +-3: AA
    |
    `-4
      |
      +-5: ACCA
      |
      `-6
        |
        +-7: ACCA
        |
        `-8
          |
          +-9: ACCA
          |
          `-10: ACGCA
-}
testNestedInsertions :: TestTree
testNestedInsertions = testCase "Insertion event nested inside a larger ancetoral insertion event" $ decorationTest tree
  where
    tree = [ ( 0, ""     , [""     ], [ 1, 2])
           , ( 1, "AA"   , ["A---A"], []     )
           , ( 2, ""     , [""     ], [ 3, 4])
           , ( 3, "AA"   , ["A---A"], []     )
           , ( 4, ""     , [""     ], [ 5, 6])
           , ( 5, "ACCA" , ["AC-CA"], []     )
           , ( 6, ""     , [""     ], [ 7, 8])
           , ( 7, "ACCA" , ["AC-CA"], []     )
           , ( 8, ""     , [""     ], [ 9,10])
           , ( 9, "ACCA" , ["AC-CA"], []     )
           , (10, "ACGCA", ["ACGCA"], []     )
           ]



{- |
  This tree contains disjoint ancestoral insertion events and branches with
  anti-symetric adjacent insertion events.

  This tree tests appropriate handling of the ordering of insertion events biasing from root to leafs.

  This tree will fail for many naive attempts at greedy insertion event application.

  0
  |
  +-1: AAA
  |
  `-2
    |
    +-3: AAA
    |
    `-4
      |
      +-5: ACACA
      |
      `-6
        |
        +-7:
        | |
        | +-13: ACACA
        | |
        | `-14
        |   |
        |   +-15: ACACA
        |   |
        |   `-16: ACAGCA
        |
        `-8
          |
          +-9: ACACA
          |
          `-10
            |
            +-11 ACACA
            |
            `-12: ACGACA
-}
testBranchesWithAdjacentInsertions :: TestTree
testBranchesWithAdjacentInsertions = testCase "Insertion events adjacent to ancetoral insertion events" $ decorationTest tree
  where
    tree = [ ( 0, ""       , [""       ], [ 1, 2])
           , ( 1, "AAA"    , ["A--A--A"], []     )
           , ( 2, ""       , [""       ], [ 3, 4])
           , ( 3, "AAA"    , ["A--A--A"], []     )
           , ( 4, ""       , [""       ], [ 5, 6])
           , ( 5, "ACACA"  , ["AC-A-CA"], []     )
           , ( 6, ""       , [""       ], [ 7, 8])
           , ( 7, ""       , [""       ], [13,14])
           , ( 8, ""       , [""       ], [ 9,10])
           , ( 9, "ACACA"  , ["AC-A-CA"], []     )
           , (10, ""       , [""       ], [11,12])
           , (11, "ACACA"  , ["AC-A-CA"], []     )
           , (12, "ACGACA" , ["ACGA-CA"], []     )
           , (13, "ACACA"  , ["AC-A-CA"], []     )
           , (14, ""       , [""       ], [15,16])
           , (15, "ACACA"  , ["AC-A-CA"], []     )
           , (16, "ACAGCA" , ["AC-AGCA"], []     )

           ]



{- |
  This tree should *crush* your very soul.

  This tree contains insertion events that are ambiguous with respect to their
  resolution. Two possible resolutions are presented: a large insertion event
  prepended to a smaller ancestral insertion event, and two insertion events that
  prepend and append another ancestral insertion event.

  This will break if the accumulation of insertion events during implied alignment
  post-order traversal is not perfectly reapplied during the pre-order traversal. It is
  possible that one of the ambiguous resolutions will be chosen on the post-order
  traversal and that the other ambiguous resolution will be chosen on the pre-order
  traversal.

  0
  |
  +-1: AA
  |
  `-2
    |
    +-3: AA
    |
    `-4
      |
      +-5: ACA
      |
      `-6
        |
        +-7: ACA
        |
        `-8
          |
          +-9 ACA
          |
          `-10: AGCTCA
-}
testAmbigousResolutionConsistency :: TestTree
testAmbigousResolutionConsistency = testCase "Consistency of ambiguous insertion event resolutions" $ decorationTest tree
  where
    tree = [ ( 0, ""       , [""      ], [ 1, 2])
           , ( 1, "AA"     , ["A----A"], []     )
           , ( 2, ""       , [""      ], [ 3, 4])
           , ( 3, "AA"     , ["A----A"], []     )
           , ( 4, ""       , [""      ], [ 5, 6])
           , ( 5, "ACA"    , ["A---CA"], []     )
           , ( 6, ""       , [""      ], [ 7, 8])
           , ( 7, "ACA"    , ["A---CA"], []     )
           , ( 8, ""       , [""      ], [ 9,10])
           , ( 9, "ACA"    , ["A---CA"], []     )
           , (10, "AGCTCA" , ["AGCTCA"], []     )
           ]
