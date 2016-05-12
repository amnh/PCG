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

module Analysis.ImpliedAlignment.Test where

import           Analysis.ImpliedAlignment.Standard
import           Bio.Character.Dynamic.Coded
import           Bio.PhyloGraph

import           Data.BitVector hiding (and)
import qualified Data.Vector    as V
import qualified Data.IntMap    as IM
import           Test.Tasty
--import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

testSuite :: TestTree
testSuite = testGroup "Implied Alignment" [numerate]


fullIA :: TestTree
fullIA = testGroup "Full alignment properties" [lenHolds]
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

numerate :: TestTree
numerate = testGroup "Numeration properties" [idHolds, lengthHolds]
    where
        idHolds = testProperty "When a sequence is numerated with itself, get indices and the same counter" checkID
        checkID :: DynamicChar -> Bool
        checkID inChar = traces == defaultH && counter == 0
            where
                defaultH = V.fromList [0..numChars inChar] 
                (traces, counter) = numerateOne gapCharacter inChar defaultH inChar 0
                gapCharacter = setBit (bitVec 0 (0 :: Integer)) (numChars inChar)

        -- TODO: make sure that these have the same alphabet
        lengthHolds = testProperty "Numerate returns a sequence of the correct length" checkLen
        checkLen :: DynamicChar -> DynamicChar -> Int -> Bool
        checkLen seq1 seq2 count = V.length traces == maxLen && counter >= count
            where 
                defaultH = V.fromList [0..numChars seq1]
                (traces, counter) = numerateOne gapCharacter seq1 defaultH seq2 count
                maxLen = maximum [numChars seq1, numChars seq2]
                gapCharacter = setBit (bitVec 0 (0 :: Integer)) (numChars seq1)

        --homologyHolds = testProperty "Homology position has expected properties: homologies has the same length as the sequence, and the counter increases"
