----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Scoring
-- Copyright   :  () 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-incomplete-record-updates #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

--{-# LANGUAGE NoMonoLocalBinds      #-}

module Analysis.Scoring
  (
  -- * Decoration
    performDecoration
  , scoreSolution
  -- * Decoration Removal
  , wipeNode
  , wipeScoring
  ) where

import           Analysis.Parsimony.Additive.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Analysis.Parsimony.Fitch.Internal
import           Analysis.Parsimony.Sankoff.Internal
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.Node.Context
import           Bio.Graph.PhylogeneticDAG                     (setDefaultMetadata)
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Lens.Operators                        ((%~), (.~), (^.))
import           Data.Default
import           Data.EdgeLength
import           Data.Function                                 ((&))
import           Data.IntMap                                   (IntMap)
import qualified Data.List.NonEmpty                            as NE
import           Data.NodeLabel
import           Data.Vector                                   (Vector)


-- |
-- Remove all scoring data from nodes.
wipeScoring
  :: Default n
  => PhylogeneticDAG m e n u v w x y z
  -> PhylogeneticDAG m e n (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
wipeScoring (PDAG2 dag m) = PDAG2 wipedDAG m
  where
    wipedDAG =
      dag & _references %~ fmap wipeDecorations
          & _graphData  %~ setDefaultMetadata

    wipeDecorations
      :: Default n
      => IndexData e (PhylogeneticNode (CharacterSequence u v w x y z) n)
      -> IndexData e (PhylogeneticNode (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n)
    wipeDecorations ind =
      ind & _nodeDecoration %~ wipeNode shouldWipe

      where
        shouldWipe = (not . null) . childRefs $ ind


-- |
-- Conditionally wipe the scoring of a single node.
wipeNode
  :: ( Default n
     )
  => Bool -- ^ Do I wipe?
  -> PhylogeneticNode (CharacterSequence        u         v         w         x         y         z ) n
  -> PhylogeneticNode (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n
wipeNode wipe =
  PNode2 <$> pure . g . NE.head . resolutions <*> f . nodeDecorationDatum2
      where
        f :: Default a => a -> a
        f | wipe      = const def
          | otherwise = id

        g res = res & _characterSequence %~ hexmap h h h h h h

        h :: a -> Maybe a
        h | wipe      = const Nothing
          | otherwise = Just



-- |
-- Take a solution of one or more undecorated trees and assign preliminary and
-- final states to all nodes.
scoreSolution :: CharacterResult -> PhylogeneticSolution FinalDecorationDAG
scoreSolution (PhylogeneticSolution forests) = PhylogeneticSolution $ fmap performDecoration <$> forests


-- |
-- Take an undecorated tree and assign preliminary and final states to all nodes.
performDecoration
  :: forall u v w x y z m .
     ( DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , RangedCharacterDecoration u ContinuousCharacter
     , RangedCharacterDecoration w StaticCharacter
     , SimpleDynamicDecoration z DynamicCharacter
     )
  => PhylogeneticDAG m EdgeLength NodeLabel (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> FinalDecorationDAG
performDecoration x =
    case length . (^. _phylogeneticForest) $ x of
      1 -> finalizeForSingleNode
          . performPreorderDecoration
          . performPostorderDecoration $ x
      _ ->
          finalizeEdgeData
        . performPreorderDecoration
        . performPostorderDecoration $ x
  where
    finalizeEdgeData :: PreorderDecorationDAG -> FinalDecorationDAG
    finalizeEdgeData = setEdgeSequences
                         (const additivePostorderPairwise)
                         (const    fitchPostorderPairwise)
                         (const additivePostorderPairwise)
                         sankoffPostorderPairwise
                         sankoffPostorderPairwise
                         adaptiveDirectOptimizationPostorderPairwise
                         contextualNodeDatum

    finalizeForSingleNode :: PreorderDecorationDAG -> FinalDecorationDAG
    finalizeForSingleNode (PDAG2 dag meta) = PDAG2 updatedDAG meta
      where
        updatedDAG = dag & _references .~ newRefs

        newRefs :: FinalReferenceVector
        newRefs = setEmptyEdgeAnnotation <$> (dag ^. _references)
        emptyEdgeAnnotation :: IntMap EdgeAnnotation
        emptyEdgeAnnotation = mempty

        setEmptyEdgeAnnotation :: IndexData e n -> IndexData EdgeAnnotation n
        setEmptyEdgeAnnotation indexData = indexData & _childRefs .~ emptyEdgeAnnotation

    performPreorderDecoration
      :: PostorderDecorationDAG
           ( TraversalTopology
           , Double
           , Double
           , Double
           , Data.Vector.Vector (NE.NonEmpty TraversalFocusEdge)
           )
      -> PreorderDecorationDAG
    performPreorderDecoration =
         preorderFromRooting
          adaptiveDirectOptimizationPreorder
          edgeCostMapping
          contextualNodeDatum
          minBlockContext

        . preorderSequence
          (const additivePreorder)
          (const fitchPreorder   )
          (const additivePreorder)
          (const sankoffPreorder )
          (const sankoffPreorder )
          (const extractPreNode  )
      where
        adaptiveDirectOptimizationPreorder meta decorationPreContext
          = directOptimizationPreorder pairwiseAlignmentFunction meta decorationPreContext
            where
              pairwiseAlignmentFunction = selectDynamicMetric meta


    performPostorderDecoration
      :: PhylogeneticDAG m EdgeLength n (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
      -> PostorderDecorationDAG
           ( TraversalTopology
           , Double
           , Double
           , Double
           , Data.Vector.Vector (NE.NonEmpty TraversalFocusEdge)
           )

    performPostorderDecoration _ =  postorderResult

    (minBlockContext, postorderResult) = assignPunitiveNetworkEdgeCost post
    (post, edgeCostMapping, contextualNodeDatum) =
         assignOptimalDynamicCharacterRootEdges adaptiveDirectOptimizationPostorder
         . postorderSequence'
             (const (g' additivePostorder))
             (const (g' fitchPostorder))
             (const (g' additivePostorder))
             (g' . sankoffPostorder)
             (g' . sankoffPostorder)
             (g' . adaptiveDirectOptimizationPostorder)
         $ x

    g' :: (PostorderContext n c -> e) -> (PostorderContext (Maybe n) c -> e)
    g' postFn = \case
      LeafContext optD ->
        case optD of
          Nothing -> error "unitialized leaf node in PostorderBinaryContext!"
          Just d  -> postFn $ LeafContext d

      PostNetworkContext _ ->
        postFn $
          PostNetworkContext
            (error "The network internal node's data is used in the postorder!")
      PostBinaryContext a b -> postFn $ PostBinaryContext a b

    adaptiveDirectOptimizationPostorder meta = directOptimizationPostorder pairwiseAlignmentFunction
      where
        pairwiseAlignmentFunction = selectDynamicMetric meta

    adaptiveDirectOptimizationPostorderPairwise meta = directOptimizationPostorderPairwise pairwiseAlignmentFunction
      where
        pairwiseAlignmentFunction = selectDynamicMetric meta
