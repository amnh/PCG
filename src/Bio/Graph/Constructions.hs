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
  , UnifiedBlock
  , UnifiedSequences
  , UnifiedCharacterBlock
  , UnifiedCharacterSequence
  , UnifiedContinuousCharacter
  , UnifiedDiscreteCharacter
  , UnifiedDynamicCharacter
  , UnifiedMetadataBlock
  , UnifiedMetadataSequence
  , UnReifiedCharacterDAG
  ) where


import Bio.Character
import Bio.Character.Decoration.Additive
import Bio.Character.Decoration.Continuous
import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Dynamic
import Bio.Character.Decoration.Fitch
import Bio.Character.Decoration.Metric
import Bio.Graph.PhylogeneticDAG.Class
import Bio.Graph.PhylogeneticDAG.Internal
import Bio.Graph.ReferenceDAG.Internal
import Bio.Graph.Solution
import Bio.Sequence
import Bio.Sequence.Metadata
import Control.Evaluation
import Data.EdgeLength
import Data.List.NonEmpty
import Data.MonoTraversable
import Data.NodeLabel
import Data.Vector (Vector)


-- |
-- A /reified/ DAG that contains characters but has no decorations.
type CharacterDAG =
       PhylogeneticDAG2
         ()
         StaticCharacter
         (Element DynamicChar)
         EdgeLength
         NodeLabel
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


-- |
-- A solution that contains only topological /and/ character information.
type CharacterResult = PhylogeneticSolution CharacterDAG


-- |
-- Simple monad transformer stack for evaluating a phylogenetic search.
type SearchState = EvaluationT IO GraphState


-- |
-- The state of the graph that partitions the evaluation model on one of two
-- paths depending on the presence or absence of character states in the search.
type GraphState = Either TopologicalResult DecoratedCharacterResult


-- |
-- A solution that contains only topological information.
-- There are no characters on which to optimize.
type TopologicalResult = PhylogeneticSolution (ReferenceDAG () EdgeLength (Maybe String))


-- |
-- Fully-decorated solution after both a post-order and a pre-order traversal.
type DecoratedCharacterResult = PhylogeneticSolution FinalDecorationDAG


-- |
-- Decoration of a phylogenetic DAG after a pre-order traversal.
type FinalDecorationDAG =
       PhylogeneticDAG2
         (TraversalTopology, Double, Double, Double, Data.Vector.Vector (NonEmpty TraversalFocusEdge))
         StaticCharacter
         (Element DynamicChar)
         EdgeLength
         NodeLabel
         (ContinuousOptimizationDecoration    ContinuousChar )
         (FitchOptimizationDecoration         StaticCharacter)
         (AdditiveOptimizationDecoration      StaticCharacter)
         (SankoffOptimizationDecoration       StaticCharacter)
         (SankoffOptimizationDecoration       StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicChar    )


-- |
-- Decoration of a phylogenetic DAG after a post-order traversal.
type PostOrderDecorationDAG m =
       PhylogeneticDAG2
         m
         StaticCharacter
         (Element DynamicChar)
         EdgeLength
         NodeLabel
         (ContinuousPostorderDecoration ContinuousChar )
         (FitchOptimizationDecoration   StaticCharacter)
         (AdditivePostorderDecoration   StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (DynamicDecorationDirectOptimizationPostOrderResult DynamicChar)


-- |
-- A "heterogenous" character block after being read in from a READ command.
type  UnifiedBlock =
    ( UnifiedMetadataBlock
    , UnifiedCharacterBlock
    )


type  UnifiedCharacterBlock
    = CharacterBlock
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


type  UnifiedMetadataBlock
    = MetadataBlock
        StaticCharacter
        (Element DynamicChar)
        ()


-- |
-- A "heterogenous" character sequence after being read in from a READ command.
type  UnifiedSequences =
    ( UnifiedMetadataSequence
    , CharacterSequence
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter
    )


type  UnifiedMetadataSequence
    = MetadataSequence
        StaticCharacter
        (Element DynamicChar)
        ()


type  UnifiedCharacterSequence
    = CharacterSequence
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


-- |
-- A continuous static character after being read in from a READ command.
-- Contains no decorations, and has not been assigned a scoring class.
-- Expected to be @Nothing@-valued for internal nodes and @Just@-valued for leaf
-- nodes.
type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousChar)


-- |
-- A discrete static character after being read in from a READ command.
-- Contains no decorations, and has not been assigned a scoring class.
-- Expected to be @Nothing@-valued for internal nodes and @Just-valued for leaf
-- nodes.
type UnifiedDiscreteCharacter   = Maybe (DiscreteDecoration StaticCharacter)


-- |
-- A dynamic character after being read in from a READ command.
-- Contains no decorations. Expected to be @Nothing@-valued for internal nodes
-- and @Just@-valued for leaf nodes.
type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicChar)


-- |
-- A DAG as read in from a READ command before being reified.
type UnReifiedCharacterDAG =
       PhylogeneticDAG
         ()
         StaticCharacter
         (Element DynamicChar)
         EdgeLength
         NodeLabel
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter
