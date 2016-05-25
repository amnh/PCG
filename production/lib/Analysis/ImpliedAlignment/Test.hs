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

import           Analysis.General.NeedlemanWunsch
import           Analysis.ImpliedAlignment.Standard
import           Bio.Character.Dynamic.Coded
import           Bio.Character.Parsed
import           Bio.PhyloGraph

import           Data.Alphabet
import           Data.BitVector (BitVector, setBit, bitVec)
import           Data.Foldable
import qualified Data.IntMap    as IM
import           Data.IntSet     (IntSet)
import           Data.List
import           Data.MonoTraversable
import qualified Data.Set as S
import qualified Data.Vector    as V
import           Test.Tasty
--import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Instances
import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "Implied Alignment"
          [ numerate
          , fullIA
          ]


fullIA :: TestTree
fullIA = testGroup "Full alignment properties" [lenHolds, twoRuns]
    where
        lenHolds = testProperty "The sequences on a tree are longer or the same at end" checkLen
        checkLen :: StandardSolution -> Bool
        checkLen inSolution = checkLS
            where 
                alignments = iaSolution inSolution
                checkLS = and $ zipWith checkLF (forests inSolution) alignments
                checkLF f fa = and $ zipWith checkLD f fa
                checkLD d a = and $ zipWith checkL (V.toList $ nodes d) (IM.toList a)
                checkL n (_, s) = V.and $ V.zipWith (\c1 c2 -> numChars c1 <= numChars c2) (getFinalGapped n) s

        twoRuns = testProperty "After two runs of IA, assignments are static" twoIA
            where
                twoIA :: StandardSolution -> Bool
                twoIA (Solution _ meta forests) = foldr (\f acc -> foldr checkStatic acc f) True forests
                    where
                        counts = (V.replicate (length meta) 0)
                        oneRun, twoRun :: DAG -> DAG
                        oneRun t = snd $ numeratePreorder t (getRoot t) meta counts
                        twoRun t = snd $ numeratePreorder (oneRun t) (getRoot $ oneRun t) meta counts 
                        checkStatic t acc = acc && (oneRun t == twoRun t)

propagation :: TestTree
propagation = testGroup "Propagation of insertions along tree" [lenHolds, homologyIncrease]
    where
        lenHolds = testProperty "After propagation, homologies have the same length" checkLen
        checkLen :: ForIA -> V.Vector IntSet -> Bool
        checkLen (tree, node) insertions = compareLens
            where
                resultTree = backPropagation tree node insertions
                compareLens = V.and $ V.zipWith (\n0 n -> V.length (getHomologies n0) == V.length (getHomologies n)) (nodes tree) (nodes resultTree)

        homologyIncrease = testProperty "After propagation, homologies are larger or the same" checkHomology
        checkHomology :: ForIA -> V.Vector IntSet -> Bool
        checkHomology = undefined

numerate :: TestTree
numerate = testGroup "Numeration properties" [idHolds, lengthHolds, counterIncrease, monotonic]
    where
        idHolds = testProperty "When a sequence is numerated with itself, get indices and the same counter" checkID
        checkID :: DynamicChar -> Bool
        checkID inChar = onull inChar || (traces == defaultH && counter <= numChars inChar)
            where
                defaultH = V.fromList [0..numChars inChar - 1] 
                (traces, counter, _) =  numerateOne inChar inChar defaultH 0

        -- TODO: Talk to Eric about numChars ()
        lengthHolds = testProperty "Numerate returns a sequence of the correct length" checkLen
        checkLen :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkLen inParse count = V.length traces >= maxLen
            where 
                (seq1, seq2) = encodeArbSameLen inParse
                defaultH     = V.fromList [0..numChars seq1 - 1]
                (traces, counter, _) = numerateOne seq1 seq2 defaultH count
                maxLen       = maximum [numChars seq1, numChars seq2]

        counterIncrease = testProperty "After numerate runs, counter is same or larger" checkCounter
        checkCounter :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkCounter inParse count = counter >= count
            where 
                (seq1, seq2) = encodeArbSameLen inParse
                defaultH     = V.fromList [0..numChars seq1 - 1]
                (traces, counter, _) = numerateOne seq1 seq2 defaultH count

        monotonic = testProperty "Numerate produces a monotonically increasing homology" checkIncrease
        checkIncrease :: (GoodParsedChar, GoodParsedChar) -> Int -> Bool
        checkIncrease inParse count = increases $ toList traces
            where 
                (seq1, seq2) = encodeArbSameLen inParse
                defaultH     = V.fromList [0..numChars seq1 - 1]
                (traces, counter, _) = numerateOne seq1 seq2 defaultH count
                increases :: Ord a => [a] -> Bool
                increases []       = True
                increases [x]      = True
                increases (x:y:xs) = x <= y && increases (y:xs)


-- | Useful function to convert encoding information to two encoded seqs
encodeArbSameLen :: (GoodParsedChar, GoodParsedChar) -> (DynamicChar, DynamicChar)
encodeArbSameLen (parse1, parse2) = (encodeDynamic alph (V.take minLen p1), encodeDynamic alph (V.take minLen p2))
    where
        (p1,p2) = (getGoodness parse1, getGoodness parse2)
        minLen  = minimum [length p1, length p2]
        oneAlph = foldMap S.fromList
        alph    = constructAlphabet $ (oneAlph p1) `S.union` (oneAlph p2)

-- | Newtyping ensures that the sequence and ambiguity groups are both non empty.
newtype GoodParsedChar
      = GoodParsedChar
      { getGoodness :: ParsedChar
      } deriving (Eq,Show)

instance Arbitrary GoodParsedChar where
  arbitrary = do
    let ambiguityGroupGenerator = listOf1 arbitrary :: Gen [String]
    someAmbiguityGroups <- listOf1 ambiguityGroupGenerator
    pure . GoodParsedChar $ V.fromList someAmbiguityGroups

type ForIA = (DAG, Node)

instance Arbitrary ForIA where
    arbitrary = undefined {-do
        someTree <- arbitrary :: Gen DAG
        let numNodes = V.length $ nodes someTree-}
