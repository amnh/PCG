-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.SearchState
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

module PCG.SearchState where

import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
-- import           Bio.Character.Decoration.NonMetric
import           Bio.Sequence
import           Bio.Sequence.Block
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Evaluation
import           Data.Key
import           Data.Monoid

-- import Debug.Trace

type SearchState = EvaluationT IO (Either TopologicalResult DecoratedCharacterResult)

--type SearchState = EvaluationT IO (Either TopologicalResult CharacterResult)

--type SearchState = EvaluationT IO (Either TopologicalResult (PhylogeneticSolution TestDecorationDAG))

type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))


type CharacterResult   = PhylogeneticSolution CharacterDAG


type DecoratedCharacterResult = PhylogeneticSolution InitialDecorationDAG


type CharacterDAG      = PhylogeneticDAG
                             (Maybe Double)
                             (Maybe String)
                             UnifiedDiscreteCharacter
                             UnifiedDiscreteCharacter
                             UnifiedContinuousCharacter
                             UnifiedDiscreteCharacter
                             UnifiedDiscreteCharacter
                             UnifiedDynamicCharacter

type InitialDecorationDAG = PhylogeneticDAG
                             (Maybe Double)
                             (Maybe String)
                             (SankoffOptimizationDecoration  StaticCharacter)
                             (SankoffOptimizationDecoration  StaticCharacter)
                             UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
                             (FitchOptimizationDecoration    StaticCharacter)
                             (AdditiveOptimizationDecoration StaticCharacter)
                             UnifiedDynamicCharacter
{-
type TestDecorationDAG = PhylogeneticDAG
                             (Maybe Double)
                             (Maybe String)
                             UnifiedDiscreteCharacter -- (SankoffOptimizationDecoration  StaticCharacter)
                             UnifiedDiscreteCharacter -- (SankoffOptimizationDecoration  StaticCharacter)
                             UnifiedContinuousCharacter --(ContinuousOptimizationDecoration ContinuousChar)
                             (FitchOptimizationDecoration    StaticCharacter)
                             (AdditiveOptimizationDecoration StaticCharacter)
                             UnifiedDynamicCharacter
-}

type  UnifiedCharacterSequence
    = CharacterSequence
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


type  UnifiedCharacterBlock
    = CharacterBlock
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousChar)


type UnifiedDiscreteCharacter   = Maybe (DiscreteDecoration StaticCharacter)


type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicChar)


-- PhylogeneticDAG (Maybe Double) (Maybe String) (Maybe StaticCharacterBlock) (Maybe DynamicChar)


data  PhylogeneticDAG e n m i c f a d
    = PDAG (ReferenceDAG e (PhylogeneticNode n (CharacterSequence m i c f a d)))


data  PhylogeneticDAG2 e n m i c f a d
    = PDAG2 (ReferenceDAG e (PhylogeneticNode2 n (CharacterSequence m i c f a d)))


instance ( Show e
         , Show n
         , Show m
         , Show i
         , Show c
         , Show f
         , Show a
         , Show d
         ) => Show (PhylogeneticDAG e n m i c f a d) where

    show (PDAG dag) =
        show dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek] ]


nodePreorderMap :: (n -> [n'] -> n')
nodePreorderMap = undefined

edgePreorderMap :: (e -> [e'] -> e')
edgePreorderMap = undefined

nodePostorderMap :: (n -> [n'] -> n')
nodePostorderMap = undefined

edgePostorderMap :: (e -> [e'] -> e')
edgePostorderMap = undefined

nodePreorderFold :: (n -> [a] -> a)
nodePreorderFold = undefined

edgePreorderFold :: (e -> [a] -> a)
edgePreorderFold = undefined

nodePostorderFold :: (n -> [a] -> a)
nodePostorderFold = undefined

edgePostorderFold :: (e -> [a] -> a)
edgePostorderFold = undefined

