-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Test
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for binary optimization
--
-----------------------------------------------------------------------------

module Analysis.Parsimony.Binary.Test where

--import           Analysis.Parsimony.Binary.DirectOptimization
import           Analysis.Parsimony.Binary.Fitch
import           Analysis.Parsimony.Binary.Internal
--import           Analysis.Parsimony.Binary.Optimization
import           Bio.Character.Dynamic
import           Bio.Metadata
import           Bio.Metadata.MaskGenerator
--import           Bio.Character.Parsed
import           Bio.PhyloGraph.Solution
import           Data.Alphabet
--import           Data.BitMatrix
--import           Data.BitVector
--import           Data.Matrix.NotStupid (getRow)
import           Data.MonoTraversable
import           Data.Monoid
import qualified Data.Vector as V
import           Test.Tasty
--import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace (trace)

standardAlph :: Alphabet String
standardAlph = fromSymbols $ V.fromList ["A", "C", "G", "T", "-"]

doMeta, fitchMeta :: CharacterMetadata DynamicChar
doMeta    = CharMeta DirectOptimization standardAlph "" False False 1 mempty (constructDynamic [], constructDynamic []) 0 costStructure
fitchMeta = CharMeta Fitch              standardAlph "" False False 1 mempty (constructDynamic [], constructDynamic []) 0 costStructure

costStructure :: CostStructure
costStructure = GeneralCost 1 1

decodeIt :: DynamicChar -> [[String]]
decodeIt = decodeDynamic standardAlph

testSuite :: TestTree
testSuite = testGroup "Binary optimization" [fitchProperties, traversalProperties]

-- I think this test for an exact values relies on implictic bias, making it not a great test?
-- | Check properties of the DO algorithm
{-
doProperties :: TestTree
doProperties = testGroup "Properties of the DO algorithm"
      [ simpleDO1
      ]
    where
        seq1    = encodeDynamic standardAlph (V.fromList [["A"], ["T"], ["T"]]) :: DynamicChar
        seq2    = encodeDynamic standardAlph (V.fromList [["A"], ["G"]])        :: DynamicChar
        result1 = naiveDO seq1 seq2 costStructure
        --expected1 :: (DynamicChar, Double, DynamicChar, DynamicChar, DynamicChar)
        expected1 = (encodeDynamic standardAlph (V.fromList [["A"], ["T", "-"], ["G", "T"]]), 2.0, encodeDynamic standardAlph (V.fromList [["A"], ["T", "-"], ["G", "T"]]), seq1, encodeDynamic standardAlph (V.fromList [["A"], ["-"], ["G"]]))
        simpleDO1 = testCase "On a simple test, DO gives expected result: ATT and AG -> A-[GT] or AG-" (expected1 @=? result1)
-}


-- | Check properties of the Fitch algorithm
fitchProperties :: TestTree
fitchProperties = testGroup "Properties of the Fitch algorithm" [preIdHolds, postIdHolds]
    where
        preIdHolds = testProperty "When Preorder Fitch runs a sequence against itself, get input as result" checkID
            where
                checkID :: DynamicChar -> Bool
                checkID inSeq = result == inSeq && cost == 0
                    where 
                        newAlph = fromSymbols $ take (stateCount $ inSeq `indexChar` 0) abstractSymbols
                        (result, _, cost) = preorderFitchBit 1 inSeq inSeq (fitchMeta {alphabet = newAlph, fitchMasks = generateMasks newAlph (olength inSeq)})

        postIdHolds = testProperty "When Postorder Fitch runs a sequence against itself, get input as result" checkID
            where
                checkID :: DynamicChar -> Bool
                checkID inSeq = result == inSeq
                    where 
                        newAlph = fromSymbols $ take (stateCount $ inSeq `indexChar` 0) abstractSymbols
                        (_, f, _) = preorderFitchBit 1 inSeq inSeq (fitchMeta {alphabet = newAlph, fitchMasks = generateMasks newAlph (olength inSeq)})
                        result = postorderFitchBit inSeq inSeq inSeq f inSeq (fitchMeta {alphabet = newAlph, fitchMasks = generateMasks newAlph (olength inSeq)})

abstractSymbols :: [String]
abstractSymbols = fmap pure . ('-' :) $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']

-- TODO: Investigate the test failure!
-- | Check properties of the traversal
traversalProperties :: TestTree
traversalProperties = testGroup "Properties of the common binary traversal" [opTwice]
    where
        opTwice = testProperty "Running an optimization twice returns same result as first time" checkTwice
            where
                checkTwice :: StandardSolution -> Bool
                checkTwice inSol = optOnce == optTwice
                    where
                        optOnce  = trace "Opt once success" $ solutionOptimization 1 inSol
                        optTwice = solutionOptimization 1 optOnce
        {-
        atLeaf = testProperty "If we start at a leaf, only that node changes" checkSimple
            where
                checkSimple :: -}
