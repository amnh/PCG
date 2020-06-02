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

module Analysis.Scoring
  (
  -- * Intermediate State
    PostorderScoringState(..)
  -- * Decoration
  , performDecoration
  , performFinalizationDecoration
  , performPostorderDecoration
  , performPreorderDecoration
  , scoreSolution
  -- * Decoration Removal
  , wipeNode
  , wipeScoring
  , wipeScoring'
  ) where

import           Analysis.Parsimony.Additive.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Analysis.Parsimony.Fitch.Internal
import           Analysis.Parsimony.Sankoff.Internal
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.Node.Context
import           Bio.Graph.PhylogeneticDAG                     (EdgeReference, setDefaultMetadata)
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Lens.Operators                        ((%~), (.~), (^.))
import           Data.Default
import           Data.EdgeLength
import           Data.Function                                 ((&))
import           Data.HashMap.Lazy                             (HashMap)
import           Data.IntMap                                   (IntMap)
import qualified Data.IntMap                                   as IntMap
import           Data.List.NonEmpty                            (NonEmpty)
import qualified Data.List.NonEmpty                            as NE
import           Data.NodeLabel
import           Data.Set                                      (Set)
import           Data.Vector                                   (Vector)
import qualified Data.Vector.NonEmpty                          as NEV



-- |
-- The context created during a post order traversal that must be preserved for
-- and consumed by the pre-order traversal.
data  PostorderScoringState a
    = PostorderScoringState
    { unusedNetworkEdges     :: Set EdgeReference
    , edgeScoringContext     :: NEV.Vector
                                  ( TraversalTopology
                                  , Double
                                  , Double
                                  , Double
                                  , Vector (NonEmpty TraversalFocusEdge)
                                  )
    , rerootingEdgeMapping   :: HashMap EdgeReference a
    , rerootingVectorMapping :: Vector (HashMap EdgeReference a)
    }


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
-- Remove all scoring data from nodes and change edge type
wipeScoring'
  :: forall m e n u v w x y z e' . Default n
  => (e -> e')
  -> PhylogeneticDAG m e n u v w x y z
  -> PhylogeneticDAG m e' n (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
wipeScoring' edgeFn (PDAG2 dag m) = PDAG2 wipedDAG m
  where
    wipedDAG =
      dag & _references %~ fmap wipeDecorations
          & _graphData  %~ setDefaultMetadata

    wipeDecorations
      :: IndexData e  (PhylogeneticNode (CharacterSequence u v w x y z) n)
      -> IndexData e' (PhylogeneticNode (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n)
    wipeDecorations ind =
      ind { childRefs      = IntMap.map edgeFn   (childRefs ind)
          , nodeDecoration = wipeNode shouldWipe (nodeDecoration ind)
          }
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
     , RangedCharacterDecoration   u ContinuousCharacter
     , RangedCharacterDecoration   w StaticCharacter
     , SimpleDynamicDecoration     z DynamicCharacter
     )
  => PhylogeneticDAG m EdgeLength NodeLabel (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> FinalDecorationDAG
performDecoration x = finalResult
  where
    (postorderState, postDAG) = performPostorderDecoration x
    preorderDAG = performPreorderDecoration postorderState postDAG
    finalResult = performFinalizationDecoration postorderState preorderDAG


performFinalizationDecoration
  :: PostorderScoringState
       (ResolutionCache
         (CharacterSequence
           u
           v
           w
           x
           y
           (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)
         )
       )
  -> PreorderDecorationDAG
  -> FinalDecorationDAG
performFinalizationDecoration postorderState preorderDAG =
    case length $ preorderDAG ^. _phylogeneticForest of
      1 -> finalizeForSingleNode preorderDAG
      _ -> finalizeEdgeData      preorderDAG
  where
    finalizeEdgeData :: PreorderDecorationDAG -> FinalDecorationDAG
    finalizeEdgeData = setEdgeSequences
                         (const additivePostorderPairwise)
                         (const    fitchPostorderPairwise)
                         (const additivePostorderPairwise)
                         sankoffPostorderPairwise
                         sankoffPostorderPairwise
                         adaptiveDirectOptimizationPostorderPairwise
                         (rerootingVectorMapping postorderState)

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

    adaptiveDirectOptimizationPostorderPairwise meta =
      let pairwiseAlignmentFunction = selectDynamicMetric meta
      in  directOptimizationPostorderPairwise pairwiseAlignmentFunction


performPostorderDecoration
  :: forall u v w x y z m .
     ( DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , RangedCharacterDecoration   u ContinuousCharacter
     , RangedCharacterDecoration   w StaticCharacter
     , SimpleDynamicDecoration     z DynamicCharacter
     )
  => PhylogeneticDAG m {- (TraversalTopology, Double, Double, Double, Vector (NonEmpty TraversalFocusEdge)) -} EdgeLength NodeLabel (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> ( PostorderScoringState
         (ResolutionCache
            (CharacterSequence
              (ContinuousPostorderDecoration ContinuousCharacter)
              (FitchOptimizationDecoration       StaticCharacter)
              (AdditivePostorderDecoration       StaticCharacter)
              (SankoffOptimizationDecoration     StaticCharacter)
              (SankoffOptimizationDecoration     StaticCharacter)
              (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)
            )
         )
     , PostorderDecorationDAG
         ( TraversalTopology
         , Double
         , Double
         , Double
         , Vector (NE.NonEmpty TraversalFocusEdge)
         )
     )
performPostorderDecoration x = (context, postorderResult)
  where
    context =
        PostorderScoringState
        { unusedNetworkEdges     = unusedEdges
        , edgeScoringContext     = minBlockContext
        , rerootingEdgeMapping   = edgeCostMapping
        , rerootingVectorMapping = contextualNodeDatum
        }
      
    (unusedEdges, minBlockContext, postorderResult) = assignPunitiveNetworkEdgeCost post
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


performPreorderDecoration
  :: PostorderScoringState
         (ResolutionCache
            (CharacterSequence
              (ContinuousPostorderDecoration ContinuousCharacter)
              (FitchOptimizationDecoration       StaticCharacter)
              (AdditivePostorderDecoration       StaticCharacter)
              (SankoffOptimizationDecoration     StaticCharacter)
              (SankoffOptimizationDecoration     StaticCharacter)
              (DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter)
            )
         )
  -> PostorderDecorationDAG
       ( TraversalTopology
       , Double
       , Double
       , Double
       , Data.Vector.Vector (NE.NonEmpty TraversalFocusEdge)
       )
  -> PreorderDecorationDAG
performPreorderDecoration postorderState = dynamicCharacterPreorder . staticCharacterPreorder
  where
    dynamicCharacterPreorder =
        preorderFromRooting
          adaptiveDirectOptimizationPreorder
          (rerootingEdgeMapping   postorderState)
          (rerootingVectorMapping postorderState)
          (edgeScoringContext     postorderState)

    staticCharacterPreorder =
        preorderSequence
          (const additivePreorder)
          (const fitchPreorder   )
          (const additivePreorder)
          (const sankoffPreorder )
          (const sankoffPreorder )
          (const extractPreNode  )
      
    adaptiveDirectOptimizationPreorder meta decorationPreContext =
        let pairwiseAlignmentFunction = selectDynamicMetric meta
        in  directOptimizationPreorder pairwiseAlignmentFunction meta decorationPreContext
        
