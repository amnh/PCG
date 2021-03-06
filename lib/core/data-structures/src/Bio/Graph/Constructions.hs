------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.Constructions
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
{-# LANGUAGE KindSignatures   #-}

module Bio.Graph.Constructions
  ( CharacterResult
  , CharacterDAG
  , CharacterNode
  , DecoratedCharacterResult
  , DecoratedCharacterNode
  , EdgeAnnotation
  , FinalDecorationDAG
  , FinalCharacterNode
  , FinalCharacterSequence
  , FinalMetadata
  , FinalReferenceVector
  , GlobalSettings
  , GraphState
  , PhylogeneticFreeDAG(..)
  , PhylogeneticDAG(..)
  , PreorderDecorationDAG
  , PostorderDecorationDAG
  , SearchState
  , TopologicalResult
  , UndecoratedReferenceDAG
  , UnifiedBlock
  , UnifiedSequences
  , UnifiedCharacterBlock
  , UnifiedCharacterNode
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
import Bio.Graph.Node
import Bio.Graph.PhylogeneticDAG.Internal
import Bio.Graph.ReferenceDAG.Internal
import Bio.Graph.Solution
import Bio.Sequence
import Control.Evaluation
import Data.EdgeLength
import Data.Kind
import Data.List.NonEmpty
import Data.NodeLabel
import Data.Vector                         (Vector)


-- |
-- A /reified/ DAG that contains characters but has no decorations.
type CharacterDAG =
       PhylogeneticDAG
         ()
         EdgeLength
         NodeLabel
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


-- |
-- A Phylogenetic Node within a 'CharacterDAG'.
type CharacterNode = PhylogeneticNode UnifiedCharacterSequence NodeLabel


-- |
-- A Phylogenetic Node within a 'FinalDecorationDAG'.
type FinalCharacterNode = PhylogeneticNode FinalCharacterSequence NodeLabel


-- |
-- A final character sequence after decoration.
type FinalCharacterSequence =
     CharacterSequence
      (ContinuousOptimizationDecoration ContinuousCharacter)
      (FitchOptimizationDecoration   StaticCharacter)
      (AdditiveOptimizationDecoration StaticCharacter)
      (SankoffOptimizationDecoration StaticCharacter)
      (SankoffOptimizationDecoration StaticCharacter)
      (DynamicDecorationDirectOptimization DynamicCharacter)


-- |
-- A Phylogenetic Node within a decorations parameterized by a functor.
type DecoratedCharacterNode (f :: Type -> Type) =
  PhylogeneticNode
    (CharacterSequence
      (f (ContinuousOptimizationDecoration ContinuousCharacter))
      (f (FitchOptimizationDecoration   StaticCharacter))
      (f (AdditiveOptimizationDecoration StaticCharacter))
      (f (SankoffOptimizationDecoration StaticCharacter))
      (f (SankoffOptimizationDecoration StaticCharacter))
      (f (DynamicDecorationDirectOptimization DynamicCharacter))
    )
    NodeLabel


-- |
-- A context type for global settings during evaluation
type GlobalSettings = ()


-- |
-- A solution that contains only topological /and/ character information.
type CharacterResult = PhylogeneticSolution CharacterDAG


-- |
-- Simple monad transformer stack for evaluating a phylogenetic search.
type SearchState = EvaluationT GlobalSettings IO GraphState


-- |
-- The state of the graph that partitions the evaluation model on one of two
-- paths depending on the presence or absence of character states in the search.
type GraphState = Either TopologicalResult DecoratedCharacterResult


-- |
-- A solution that contains only topological information.
-- There are no characters on which to optimize.
type TopologicalResult = PhylogeneticSolution (ReferenceDAG () EdgeLength (Maybe NodeLabel))


-- |
-- A DAG which has no decoration performed on it.
-- This is the graph state after reading in data.
type UndecoratedReferenceDAG = ReferenceDAG () EdgeLength (Maybe NodeLabel)


-- |
-- Fully-decorated solution after both a post-order and a pre-order traversal.
type DecoratedCharacterResult = PhylogeneticSolution FinalDecorationDAG


-- |
-- The Metadata on a 'FinalDecorationDAG' after a pre-order traversal.
type FinalMetadata = (TraversalTopology, Double, Double, Double, Data.Vector.Vector (NonEmpty TraversalFocusEdge))


-- |
-- The Reference Vector on a 'FinalDecorationDAG' after a pre-order traversal.
type FinalReferenceVector = Vector (IndexData EdgeAnnotation FinalCharacterNode)

-- |
-- Decoration of a phylogenetic DAG after a pre-order traversal AND after the edge data has been finalized.
type FinalDecorationDAG =
       PhylogeneticDAG
         FinalMetadata
         EdgeAnnotation
         NodeLabel
         (ContinuousOptimizationDecoration ContinuousCharacter)
         (FitchOptimizationDecoration          StaticCharacter)
         (AdditiveOptimizationDecoration       StaticCharacter)
         (SankoffOptimizationDecoration        StaticCharacter)
         (SankoffOptimizationDecoration        StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicCharacter)


-- |
-- Decoration of a phylogenetic DAG after a pre-order traversal.
type PreorderDecorationDAG =
       PhylogeneticDAG
         FinalMetadata
         EdgeLength
         NodeLabel
         (ContinuousOptimizationDecoration ContinuousCharacter)
         (FitchOptimizationDecoration          StaticCharacter)
         (AdditiveOptimizationDecoration       StaticCharacter)
         (SankoffOptimizationDecoration        StaticCharacter)
         (SankoffOptimizationDecoration        StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicCharacter)

-- |
-- Decoration of a phylogenetic DAG after a post-order traversal.
type PostorderDecorationDAG m =
       PhylogeneticDAG
         m
         EdgeLength
         NodeLabel
         (ContinuousPostorderDecoration ContinuousCharacter)
         (FitchOptimizationDecoration       StaticCharacter)
         (AdditivePostorderDecoration       StaticCharacter)
         (SankoffOptimizationDecoration     StaticCharacter)
         (SankoffOptimizationDecoration     StaticCharacter)
         (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)

-- |
-- A "heterogeneous" character block after being read in from a READ command.
type  UnifiedBlock =
    ( UnifiedMetadataBlock
    , UnifiedCharacterBlock
    )


-- |
-- A character block resulting from the READ command.
type  UnifiedCharacterBlock
    = CharacterBlock
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


-- |
-- A metadata block resulting from the READ command.
type  UnifiedMetadataBlock = MetadataBlock ()


-- |
-- A "heterogeneous" character sequence after being read in from a READ command.
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


-- |
-- A character sequence resulting from the READ command.
type  UnifiedCharacterSequence
    = CharacterSequence
        UnifiedContinuousCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDiscreteCharacter
        UnifiedDynamicCharacter


-- |
-- A metadata sequence resulting from the READ command.
type  UnifiedMetadataSequence = MetadataSequence ()


-- |
-- A Phylogenetic Node within a unified character DAG.

type UnifiedCharacterNode = PhylogeneticFreeNode NodeLabel UnifiedCharacterSequence


-- |
-- A continuous static character after being read in from a READ command.
-- Contains no decorations, and has not been assigned a scoring class.
-- Expected to be @Nothing@-valued for internal nodes and @Just@-valued for leaf
-- nodes.
type UnifiedContinuousCharacter = Maybe (ContinuousDecorationInitial ContinuousCharacter)


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
type UnifiedDynamicCharacter    = Maybe (DynamicDecorationInitial DynamicCharacter)


-- |
-- A DAG as read in from a READ command before being reified.
type UnReifiedCharacterDAG =
       PhylogeneticFreeDAG
         ()
         EdgeLength
         NodeLabel
         UnifiedContinuousCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDiscreteCharacter
         UnifiedDynamicCharacter


-- |
-- An annotation for an edge memoizing certain data from finalized node decorations.
type EdgeAnnotation =
    ( EdgeLength
    , CharacterSequence
        (ContinuousPostorderDecoration ContinuousCharacter)
        (FitchOptimizationDecoration       StaticCharacter)
        (AdditivePostorderDecoration       StaticCharacter)
        (SankoffOptimizationDecoration     StaticCharacter)
        (SankoffOptimizationDecoration     StaticCharacter)
        (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)
    )



{-
extractReferenceDAG
  :: Either TopologicalResult DecoratedCharacterResult
  -> ReferenceDAG () EdgeLength (Maybe NodeLabel)
extractReferenceDAG = either extractTopResult extractRefDAGfromDec
  where
    extractTopResult = extractSolution



extractRefDAGfromDec
  :: DecoratedCharacterResult -> ReferenceDAG () EdgeLength (Maybe NodeLabel)
extractRefDAGfromDec finalDecDAG =
  let
    decRefDAG        = finalDecDAG      & (^. _phylogeneticForest) . extractSolution
    refDAGNoMetadata = decRefDAG        & (_graphData . _graphMetadata) .~ ()
    refDAGNoEdgeData = refDAGNoMetadata & (_references . mapped . _childRefs . mapped) %~ fst
    refDAG           = refDAGNoEdgeData &
                         (_references . mapped . _nodeDecoration) %~ Just . nodeDecorationDatum2
  in refDAG
-}
