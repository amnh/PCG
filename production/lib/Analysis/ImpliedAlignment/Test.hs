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
import           Bio.PhyloGraph
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
import qualified Data.Vector       as V
import           Test.Custom.Tree
--import qualified Test.Custom.Types as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances
import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "Implied Alignment"
          [ testNumerate
          , testImpliedAlignmentCases
          , fullIA
          ]

fullIA :: TestTree
fullIA = testGroup "Full alignment properties" [ lenHoldsTest
--                                               , checkDOResult1
--                                               , checkIAResult1
--                                               , checkDOResult2
--                                               , checkIAResult2
                                               ]
    where
        lenHoldsTest       = testProperty "The sequences on a tree are longer or the same at end." checkLen

{-
        checkDOResult1 = testCase "On a simple cherry, DO behaves as expected" (expectedDO @=? doResult1)
            where
                doResult1 = allOptimization 1 (pure doMeta) cherry1
                cherry1   = V.fromList [rootTest, leftTest, rightTest]

        checkDOResult2 = testCase "On a slightly larger case, DO behaves as expected" (expectedDO2 @=? doResult2)
            where
                doResult2    = allOptimization 1 (pure doMeta) longerTest
                longerTest   = V.fromList [rootTest, leftTest2, rightTest, node3, node4] 

        checkIAResult1 = testCase "On the same cherry, IA gives the expected result" (expectedIA1 @=? iaResult1)
            where
                iaResult1   = impliedAlign expectedDO (pure doMeta)
                expectedIA1 = IM.fromList [ (1, encodeThem . pure $ V.fromList 
                                             [ ["A"]
                                             , ["T"]
                                             , ["T"]
                                             , ["-"]
                                             ])
                                          , (2, encodeThem . pure $ V.fromList 
                                             [ ["A"]
                                             , ["-"]
                                             , ["-"]
                                             , ["G"]
                                             ])
                                          ]

        checkIAResult2 = testCase "On that larger case, IA gives the expected result" (expectedIA2 @=? iaResult2)
            where
                iaResult2   = impliedAlign expectedDO2 (pure doMeta)
                expectedIA2 = IM.fromList [ (2, encodeThem . pure $ V.fromList 
                                             [["A"], ["-"], ["-"], ["G"], ["-"]])
                                          , (3, encodeThem . pure $ V.fromList 
                                             [["A"], ["T"], ["-"], ["-"], ["T"]])
                                          , (4, encodeThem . pure $ V.fromList 
                                             [["A"], ["T"], ["A"], ["-"], ["G"]])
                                          ]

        bioAlph        = constructAlphabet . V.fromList . fmap pure $ "ACGT-"
        encodeThem     = V.fromList . fmap (encodeDynamic bioAlph)
        doMeta         = CharMeta DirectOptimization bioAlph "" False False 1 mempty (emptyChar, emptyChar) 0 (GeneralCost 1 1)

        rootTest       = T.TestNode 0 True False [] [1,2] mempty mempty mempty mempty mempty mempty mempty 0 0
        leftTest       = rootTest { T.code = 1
                                  , T.isRoot = False
                                  , T.isLeaf = True
                                  , T.parents = [0]
                                  , T.children = []
                                  , T.encoded = encodeThem . pure $ V.fromList 
                                      [["A"], ["T"], ["T"]]
                                  }
        rightTest      = leftTest { T.code = 2
                                  , T.encoded = encodeThem . pure $ V.fromList 
                                      [ ["A"], ["G"] ]
                                  }
        leftTest2      = leftTest { T.isLeaf = False
                                  , T.children = [3, 4]
                                  , T.encoded = mempty
                                  }
        seq2a          = encodeThem . pure $ V.fromList [ ["A"]
                                                        , ["T"]
                                                        , ["A", "-"]
                                                        , ["T", "G"]
                                                        ]
        seq2b          = encodeThem . pure $ V.fromList [ ["A"]
                                                        , ["T"]
                                                        , ["G"]
                                                        ]
        seq2c          = encodeThem . pure $ V.fromList [ ["A"]
                                                        , ["T", "-"]
                                                        , ["G"]
                                                        ]
        expectedDO     = V.fromList [newRoot, leftTest, rightTest]
            where
                newRoot     = rootTest { T.preliminary = expectedSeq
                                       , T.aligned = expectedSeq
                                       , T.localCost = 2
                                       , T.totalCost = 2
                                       }
                expectedSeq = encodeThem . pure $ V.fromList [["A"], ["T", "-"], ["T", "G"]]

        expectedDO2  = V.fromList [ expectedRoot
                                  , expectedLeft
                                  , rightTest
                                  , node3
                                  , node4
                                  ]

        expectedLeft = leftTest2 { T.preliminary = seq2a
                                 , T.aligned     = seq2a
                                 , T.final       = seq2b
                                 , T.localCost   = 2
                                 , T.totalCost   = 2
                                 , T.gapped      = encodeThem . pure $ V.fromList 
                                                     [ ["A"], ["T"], ["-"], ["G"] ]
                                 }
        expectedRoot = rootTest { T.preliminary = seq2c
                                , T.aligned     = encodeThem . pure $ V.fromList 
                                                    [ ["A"], ["T", "-"], ["-"], ["G"] ]
                                , T.localCost   = 1
                                , T.totalCost   = 3
                                }

        node3        = leftTest { T.code = 3, T.parents = [1] }
        node4        = node3 { T.code    = 4
                             , T.encoded = encodeThem . pure $ V.fromList 
                                             [ ["A"], ["T"], ["A"], ["G"] ]
                             }


-}
checkLen :: StandardSolution -> Bool
checkLen inSolution = checkLS
  where 
    alignments = iaSolution $ solutionOptimization 1 inSolution
    checkLS    = and $ zipWith checkLF (forests inSolution) alignments
      where
        checkLF f fa = and $ zipWith checkLD f fa
          where
            checkLD d a = and $ zipWith checkL (V.toList $ nodes d) (IM.toList a)
              where
                checkL n (_, s) = and $ V.zipWith ((<=) `on` olength) (getFinalGapped n) s

testNumerate :: TestTree
testNumerate = testGroup "Numeration properties" [ idHolds
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

testImpliedAlignmentCases :: TestTree
testImpliedAlignmentCases = testGroup "Explicit test cases for implied alignment"
    [ testDeletedInsertion
    , testPrependedInsertions
    , testSimpleInsertionDeletionBiasing
    ]
  where
    performImpliedAlignment = (deriveImpliedAlignments defMeta . allOptimization 1 defMeta)

    decorationTest :: Foldable t => t (Int, String, [String], [Int]) -> Assertion
    decorationTest          = simpleTreeCharacterDecorationEqualityAssertion 0 "ACGT-" performImpliedAlignment getHomologies'

    testDeletedInsertion = testCase "Deletion event of an insertion event" $ decorationTest tree
      where
        tree = [ ( 0,       "", [""      ], [ 1, 2])
               , ( 1,   "AGTT", ["AG--TT"], []     )
               , ( 2,       "", [""      ], [ 3, 4])
               , ( 3,   "AGTT", ["AG--TT"], []     )
               , ( 4,       "", [""      ], [ 5, 6])
               , ( 5,   "AGTT", ["AG--TT"], []     )
               , ( 6,       "", [""      ], [ 7, 8])
               , ( 7, "AGCCTT", ["AGCCTT"], []     )
               , ( 8,       "", [""      ], [ 9,10])
               , ( 9, "AGCCTT", ["AGCCTT"], []     )
               , (10,       "", [""      ], [11,12])
               , (11, "AGCCTT", ["AGCCTT"], []     )
               , (12,       "", [""      ], [13,14])
               , (13,   "AGTT", ["AG--TT"], []     )
               , (14,   "AGTT", ["AG--TT"], []     )
               ]

    testPrependedInsertions = testCase "Prepended insertion of a deletion event" $ decorationTest tree
      where
        tree = [ ( 0, ""      , [""      , ""      , ""      ], [1, 2])
               , ( 1, "CAATTT", ["CAATTT", "CAATTT", "CAATTT"], []    )
               , ( 2, ""      , [""      , ""      , ""      ], [3, 4])
               , ( 3, "AATTT" , ["-AATTT", "-AATTT", "-AATTT"], []    )
               , ( 4, ""      , [""      , ""      , ""      ], [5, 6])
               , ( 5, "AATTT" , ["-AATTT", "-AATTT", "-AATTT"], []    )
               , ( 6, ""      , [""      , ""      , ""      ], [7, 8])
               , ( 7, "AATT"  , ["-AA-TT", "-AAT-T", "-AATT-"], []    ) -- Multiple valid gap locations here.
               , ( 8, ""      , [""      , ""      , ""      ], [9,10])
               , ( 9, "AATTT" , ["-AATTT", "-AATTT", "-AATTT"], []    )
               , (10, "AATTT" , ["-AATTT", "-AATTT", "-AATTT"], []    )
               , (11, "AATTT" , ["-AATTT", "-AATTT", "-AATTT"], []    )
               , (12, ""      , [""      , ""      , ""      ], [0,11])
               ]

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
