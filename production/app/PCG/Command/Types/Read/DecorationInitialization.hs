-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Read.DecorationInitialization
-- Copyright   :  () 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Types.Read.DecorationInitialization where

import           Analysis.Parsimony.Additive.Internal
import           Analysis.Parsimony.Fitch.Internal
import           Analysis.Parsimony.Sankoff.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Analysis.Parsimony.Dynamic.SequentialAlign

import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
--import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
--import           Bio.Character.Decoration.NonMetric

--import           Bio.Character.Encodable
--import           Bio.Character.Exportable
--import           Bio.Character.Decoration.Continuous hiding (characterName)
--import           Bio.Character.Decoration.Discrete   hiding (characterName)
--import           Bio.Character.Decoration.Dynamic    hiding (characterName)
--import           Bio.Character.Parsed
import           Bio.Sequence
--import           Bio.Sequence.Block
--import           Bio.Metadata.CharacterName hiding (sourceFile)
--import           Bio.Metadata.Parsed
--import           Bio.PhyloGraph.Solution    hiding (parsedChars)
--import           Bio.PhyloGraph.DAG
--import           Bio.PhyloGraph.Forest.Parsed
import           Bio.PhyloGraphPrime
--import           Bio.PhyloGraphPrime.Component
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Bio.PhyloGraphPrime.PhylogeneticDAG
import           Control.DeepSeq
import           Control.Lens
--import           Control.Arrow                     ((&&&))
--import           Control.Applicative               ((<|>))
--import           Data.Alphabet
import           Data.Bifunctor                    (second)
--import           Data.Foldable
--import qualified Data.IntSet                as IS
import           Data.Key
--import           Data.List                         (transpose, zip4)
import           Data.List.NonEmpty                (NonEmpty( (:|) ))
--import qualified Data.List.NonEmpty         as NE
--import           Data.List.Utility                 (duplicates)
--import           Data.Map                          (Map, intersectionWith, keys)
--import qualified Data.Map                   as Map
--import           Data.Maybe                        (catMaybes, fromMaybe, listToMaybe)
import           Data.MonoTraversable (Element)
--import           Data.Semigroup                    ((<>))
--import           Data.Semigroup.Foldable
--import           Data.Set                          (Set, (\\))
--import qualified Data.Set                   as Set
--import           Data.TCM                          (TCM)
--import qualified Data.TCM                   as TCM
--import           Data.MonoTraversable
--import           Data.Vector                       (Vector)
--import           PCG.Command.Types.Read.Unification.UnificationError
--import           PCG.SearchState
import           Prelude                    hiding (lookup, zip, zipWith)

import Debug.Trace


{-
traceOpt :: [Char] -> a -> a
traceOpt identifier x = (trace ("Before " <> identifier) ())
                  `seq` (let !v = x
                         in v `seq` (trace ("After " <> identifier) v)
                        )
-}

-- | sequentialAlignOverride, iff True forces seqAlign to run; otherwise, DO runs.
sequentialAlignOverride :: Bool
sequentialAlignOverride = True

chooseDirectOptimizationComparison :: ( SimpleDynamicDecoration d  c
                                      , SimpleDynamicDecoration d' c
                                      , Exportable c
                                      , Show c
                                      , Show (Element c)
                                      , Integral (Element c)
                                      )
                                   => d
                                   -> [d']
                                   -> c
                                   -> c
                                   -> (c, Double, c, c, c)
chooseDirectOptimizationComparison dec decs =
    case decs of
      []  -> selectBranch dec
      x:_ -> selectBranch x
  where
    selectBranch x | trace (show . length $ x ^. characterAlphabet) False = undefined
    selectBranch candidate
      | sequentialAlignOverride = sequentialAlign (candidate ^. sparseTransitionCostMatrix)
      | otherwise =
          case candidate ^. denseTransitionCostMatrix of
            Just  d -> \x y -> foreignPairwiseDO x y d
            Nothing ->
              let !scm = (candidate ^. symbolChangeMatrix)
              in \x y -> naiveDO x y scm


{-
--initializeDecorations2 :: CharacterResult -> PhylogeneticSolution InitialDecorationDAG
initializeDecorations2 (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests
  where
--    performDecoration :: CharacterDAG -> InitialDecorationDAG
    performDecoration = assignOptimalDynamicCharacterRootEdges dynamicScoring2 .
      postorderSequence'
        (g  sankoffPostOrder)
        (g  sankoffPostOrder)
        id2
        (g    fitchPostOrder)
        (g additivePostOrder)
        (g adaptiveDirectOptimizationPostOrder)
      where
        g _  Nothing  [] = error $ "Uninitialized leaf node. This is bad!"
        g h (Just  v) [] = h v []
        g h        e  xs = h (error $ "We shouldn't be using this value." ++ show e ++ show (length xs)) xs

        id2 x _ = x
        dynamicScoring  = directOptimizationPostOrder (\x y -> naiveDOConst x y undefined)
        -- Because of monomophism BS
        dynamicScoring2 = directOptimizationPostOrder (\x y -> naiveDOConst x y undefined)
{--
        adaptiveDirectOptimizationPostOrder _ _ | trace "DO call" False = undefined
        adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs
--}


{-
        postOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = postOrderLogic (sequenceDecoration parentalNode) (sequenceDecoration <$> childNodes)
          }

        preOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = preOrderLogic (sequenceDecoration parentalNode) (second sequenceDecoration <$> childNodes)
          }
-}
-}
{--}
initializeDecorations :: CharacterResult -> PhylogeneticSolution InitialDecorationDAG
initializeDecorations (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests
  where
--    performDecoration :: CharacterDAG -> InitialDecorationDAG
    performDecoration (PDAG dag) = PDAG . nodePreOrder preOrderTransformation $ nodePostOrder postOrderTransformation dag
      where
        postOrderTransformation parentalNode childNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum parentalNode)
          , sequenceDecoration  = postOrderLogic (sequenceDecoration parentalNode) (sequenceDecoration <$> childNodes)
          }

        preOrderTransformation childNode parentNodes =
          PNode
          { nodeDecorationDatum = (nodeDecorationDatum childNode)
          , sequenceDecoration  = preOrderLogic (sequenceDecoration childNode) (second sequenceDecoration <$> parentNodes)
          }

    postOrderLogic :: CharacterSequence
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedContinuousCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDynamicCharacter
      -> [ CharacterSequence
             (SankoffOptimizationDecoration StaticCharacter)
             (SankoffOptimizationDecoration StaticCharacter)
             (ContinuousPostorderDecoration ContinuousChar ) --(ContinuousOptimizationDecoration ContinuousChar)
             (FitchOptimizationDecoration   StaticCharacter)
             (AdditivePostorderDecoration   StaticCharacter)
             (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar) -- UnifiedDynamicCharacter
         ]
      -> CharacterSequence
           (SankoffOptimizationDecoration StaticCharacter)
           (SankoffOptimizationDecoration StaticCharacter)
           (ContinuousPostorderDecoration ContinuousChar ) --(ContinuousOptimizationDecoration ContinuousChar)
           (FitchOptimizationDecoration   StaticCharacter)
           (AdditivePostorderDecoration   StaticCharacter)
           (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar) -- UnifiedDynamicCharacter

    postOrderLogic currentCharSeq childCharSeqs =
        hexZipWith
          (g  sankoffPostOrder)
          (g  sankoffPostOrder)
          (g additivePostOrder)
          (g    fitchPostOrder)
          (g additivePostOrder)
          (g adaptiveDirectOptimizationPostOrder)
          currentCharSeq
          childCharSeqs'
      where
        id2 x _ = x
        childCharSeqs' =
            case childCharSeqs of
              x:xs -> hexTranspose $ x:|xs
              []   -> let c = const []
                      in hexmap c c c c c c currentCharSeq
        g _  Nothing  [] = error $ "Uninitialized leaf node. This is bad!"
        g h (Just  v) [] = h v []
        g h        e  xs = h (error $ "We shouldn't be using this value." ++ show e ++ show (length xs)) xs

        adaptiveDirectOptimizationPostOrder dec kidDecs = directOptimizationPostOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec kidDecs

{-
    preOrderLogic ::
        CharacterSequence
          (SankoffOptimizationDecoration  StaticCharacter)
          (SankoffOptimizationDecoration  StaticCharacter)
          UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
          (FitchOptimizationDecoration    StaticCharacter)
          (AdditiveOptimizationDecoration StaticCharacter)
          UnifiedDynamicCharacter
      -> [ (Word
           , CharacterSequence
               (SankoffOptimizationDecoration  StaticCharacter)
               (SankoffOptimizationDecoration  StaticCharacter)
               UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
               (FitchOptimizationDecoration    StaticCharacter)
               (AdditiveOptimizationDecoration StaticCharacter)
               UnifiedDynamicCharacter
           )
         ]
      -> CharacterSequence
           (SankoffOptimizationDecoration  StaticCharacter)
           (SankoffOptimizationDecoration  StaticCharacter)
           UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
           (FitchOptimizationDecoration    StaticCharacter)
           (AdditiveOptimizationDecoration StaticCharacter)
           UnifiedDynamicCharacter
-}
    preOrderLogic currentCharSeq parentCharSeqs =
        hexZipWith
          sankoffPreOrder
          sankoffPreOrder
          additivePreOrder
          fitchPreOrder
          additivePreOrder
          adaptiveDirectOptimizationPreOrder
          currentCharSeq
          parentCharSeqs'
      where
        id2 x _ = x
{-
        parentCharSeqs' :: CharacterSequence
                               [(Word, SankoffOptimizationDecoration   StaticCharacter)]
                               [(Word, SankoffOptimizationDecoration   StaticCharacter)]
                               [(Word, UnifiedContinuousCharacter)]
                               [(Word, FitchOptimizationDecoration     StaticCharacter)]
                               [(Word, AdditiveOptimizationDecoration  StaticCharacter)]
                               [(Word, DynamicDecorationDirectOptimization DynamicChar)]
-}
        parentCharSeqs' =
            case parentCharSeqs of
              []   -> let c = const []
                      in hexmap c c c c c c currentCharSeq
              x:xs -> let f = zip (fst <$> (x:xs))
                      in hexmap f f f f f f . hexTranspose $ snd <$> x:|xs

        adaptiveDirectOptimizationPreOrder dec kidDecs = directOptimizationPreOrder pairwiseAlignmentFunction dec kidDecs
          where
            pairwiseAlignmentFunction = chooseDirectOptimizationComparison dec $ snd <$> kidDecs

{--}


