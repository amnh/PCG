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
    , testNonHomology
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


{- |
  A set of test trees which insertion events are later deleted. These events
  should /not/ be non-homology events.
-}
testDeletedInsertions :: TestTree
testDeletedInsertions = testGroup "Deletion of insertion events (no non-homology events)"
    [ testDeletedInsertionSingle
    , testDeletedInsertionGroupMiddle
    , testDeletedInsertionGroupPrepend
    , testDeletedInsertionGroupAppend
    , testDoubleDeletedInsertion
    ]


{- |
  A set of test trees which deletion events are later filled with an insertion
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


{- |
  This tree should contain an insertion event and a deletion event which are
  adjacent. We need to make sure that these events are calculated correctly.

  It should be noted that we use a unique cost structure to force the adjacentcy
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
  This tree should contain an insertion event and a deletion event which are
  adjacent. We need to make sure that these events are calculated correctly.

  It should be noted that we use a unique cost structure to force the adjacentcy
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
  This tree should contain an two insertion events on each branch between the
  AAA and ACATA characters followed by a deletion event between the
  AAA and AATA/ACAA characters.

  We should note that both insertion events of C & T on each side of the root
  note between the AAA and ACATA characters are /non-homologous/ and should
  result in anti-symetric gaps in the leaf node alignments.

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
  This tree should contain one deletion event followwed by an insertion event.

  We should note that the deletion event and subsequent insertion event of the
  T baseare /non-homologous/!

  The presense of an additional T base in all the leaf characters tests that
  the implied alignment correctly handles the base ambiguity appropriately. 

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

  The presense of an additional T base in all the leaf characters tests that
  the implied alignment correctly handles the base ambiguity appropriately.

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
  AA and ATA characters followed.

  We should note that these insertion events of the T base on each side of the
  root note between the AA and ATA characters are /non-homologous/ and should
  result in anti-symetric gaps in the leaf node alignments.

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
  This tree should contain one insertion event on each branch between the
  ATA and ATTA characters followed.

  We should note that these insertion events of the T base on each side of the
  root note between the ATA and ATTA characters are /non-homologous/ and should
  result in anti-symetric gaps in the leaf node alignments.

  The presense of an additional T base in all the leaf characters tests that
  the implied alignment correctly handles the base ambiguity appropriately.

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


{- |
  This tree should contain three insertion events.

  We should notice that the two furthers leaf nodes contain a different number
  of insertion events. This induced a truncation issue because the insertion
  events were not being applied correctly. on the preorder pass.

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
