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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Analysis.ImpliedAlignment.Test where

import qualified Analysis.ImpliedAlignment.InsertionEvents.Test as IE (testSuite)
import           Analysis.ImpliedAlignment.Standard
import           Analysis.ImpliedAlignment.Test.Trees
import           Analysis.Parsimony.Binary.Internal
import           Bio.Character.Encodable
import           Bio.Character.Parsed
import           Bio.Metadata
import           Data.Alphabet
import           Data.Foldable
import           Data.Function                                  (on)
import qualified Data.List.NonEmpty                             as NE
import           Data.MonoTraversable
import qualified Data.Set                                       as S
import           Data.Vector                                    (Vector)
import qualified Data.Vector                                    as V
import           Test.Custom
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck


defMeta :: Vector (CharacterMetadata s)
defMeta = pure CharMeta
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

testSuite :: TestTree
testSuite = testGroup "Implied Alignment"
    [ testNumerate
    , testSupportingDataStructures
    , testImpliedAlignmentCases
--    , fullIA
    ]

testSupportingDataStructures :: TestTree
testSupportingDataStructures = testGroup "Supporting data-structures for implied alignment"
    [ IE.testSuite
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

    lengthHolds                      = testProperty "Numerate returns a sequence of the correct length" checkLen
    checkLen :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
    checkLen inParse count           = length traces >= maxLen
      where
         (seq1, seq2)        = encodeArbSameLen inParse
         (traces, (_, _), _) = numerateOne seq1 seq2 (olength seq1, count)
         maxLen              = maximum [olength seq1, olength seq2]

    counterIncrease                   = testProperty "After numerate runs, counter is same or larger" checkCounter
    checkCounter :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
    checkCounter inParse count        = counter >= count
      where
        (seq1, seq2)              = encodeArbSameLen inParse
        (_, (_, counter), _) = numerateOne seq1 seq2 (olength seq1, count)

    monotonic = testProperty "Numerate produces a monotonically increasing homology" checkIncrease
    checkIncrease :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
    checkIncrease inParse count       = increases $ toList traces
      where
        (seq1, seq2)         = encodeArbSameLen inParse
        (traces, _, _) = numerateOne seq1 seq2 (olength seq1, count)
        increases :: Ord a => [a] -> Bool
        increases []       = True
        increases [_]      = True
        increases (x:y:xs) = x < y && increases (y:xs)


-- | Useful function to convert encoding information to two encoded seqs
encodeArbSameLen :: (GoodParsedChar, GoodParsedChar) -> (DynamicChar, DynamicChar)
encodeArbSameLen (parse1, parse2) =
    ( encodeStream alph . NE.fromList $ NE.take minLen p1
    , encodeStream alph . NE.fromList $ NE.take minLen p2
    )
  where
    (p1,p2) = (getGoodness parse1, getGoodness parse2)
    minLen  = minimum [length p1, length p2]
    oneAlph = foldMap (S.fromList . toList)
    alph    = fromSymbols $ oneAlph p1 `S.union` oneAlph p2


-- | Newtyping ensures that the sequence and ambiguity groups are both non-empty.
newtype GoodParsedChar
      = GoodParsedChar
      { getGoodness :: ParsedChar
      } deriving (Eq,Show)


instance Arbitrary GoodParsedChar where
    arbitrary = do
        symbols                     <- getNonEmpty <$> arbitrary :: Gen [String]
        let ambiguityGroupGenerator =  NE.fromList <$> (sublistOf symbols `suchThat` (not . null))
        someAmbiguityGroups         <- NE.fromList <$> listOf1 ambiguityGroupGenerator
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
insertionDeletionTest rootRef symbols spec _parentRef _childRef _expectedDeletions _expectedInsertions = undefined
  where
    _inputTree  = createSimpleTree rootRef symbols spec
    _outputTree = allOptimization 1 defMeta _inputTree
