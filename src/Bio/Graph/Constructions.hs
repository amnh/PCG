------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Constructions
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Graph.Constructions
  ( CharacterResult
  , CharacterDAG
  , DecoratedCharacterResult
  , FinalDecorationDAG
  , GraphState
  , PhylogeneticDAG(..)
  , PhylogeneticDAG2(..)
  , PhylogeneticDAGish(..)
  , PostOrderDecorationDAG
  , SearchState
  , TopologicalResult
  , UnifiedCharacterSequence
  , UnifiedCharacterBlock
  , UnifiedContinuousCharacter
  , UnifiedDiscreteCharacter
  , UnifiedDynamicCharacter
  , UnReifiedCharacterDAG
  ) where


import Bio.Character
import Bio.Character.Decoration.Additive
import Bio.Character.Decoration.Continuous
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Dynamic
import Bio.Character.Decoration.Fitch
import Bio.Character.Decoration.Metric
--import Bio.Graph.Node
import Bio.Graph.PhylogeneticDAG.Class
import Bio.Graph.PhylogeneticDAG.Internal
import Bio.Graph.ReferenceDAG.Internal
import Bio.Graph.Solution
import Bio.Sequence
import Control.Evaluation
import Data.EdgeLength
import Prelude            hiding (zipWith)



type CharacterDAG =
       PhylogeneticDAG2
         EdgeLength
         (Maybe String)
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type CharacterResult = PhylogeneticSolution CharacterDAG


--type Cost = Double


type SearchState = EvaluationT IO GraphState


type GraphState = Either TopologicalResult DecoratedCharacterResult


type TopologicalResult = PhylogeneticSolution (ReferenceDAG () EdgeLength (Maybe String))


type DecoratedCharacterResult = PhylogeneticSolution FinalDecorationDAG


type FinalDecorationDAG =
       PhylogeneticDAG2
         EdgeLength
         (Maybe String)
         (ContinuousOptimizationDecoration    ContinuousChar)
         (FitchOptimizationDecoration         StaticCharacter)
         (AdditiveOptimizationDecoration      StaticCharacter)
         (SankoffOptimizationDecoration       StaticCharacter)
         (SankoffOptimizationDecoration       StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicChar)
--         (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


--type IncidentEdges = [EdgeReference]


type PostOrderDecorationDAG =
       PhylogeneticDAG2
         EdgeLength
         (Maybe String)
         (ContinuousPostorderDecoration ContinuousChar )
         (FitchOptimizationDecoration   StaticCharacter)
         (AdditivePostorderDecoration   StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


{-
type ReRootedEdgeContext u v w x y z =
   ( ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   , ResolutionCache (CharacterSequence u v w x y z)
   )
-}


type UnifiedCharacterBlock
     = CharacterBlock
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type UnifiedCharacterSequence
     = CharacterSequence
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousChar)


type UnifiedDiscreteCharacter   = Maybe (DiscreteDecoration StaticCharacter)


type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicChar)


type UnReifiedCharacterDAG =
       PhylogeneticDAG
         EdgeLength
         (Maybe String)
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter
