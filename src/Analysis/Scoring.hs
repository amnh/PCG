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

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Analysis.Parsimony.Fitch.Internal
import           Analysis.Parsimony.Sankoff.Internal
import           Analysis.Parsimony.Dynamic.DirectOptimization
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Dynamic
import           Bio.Graph
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import Control.Lens
import           Data.Default
import           Data.EdgeLength
import qualified Data.List.NonEmpty as NE
import           Data.MonoTraversable      (Element)
import           Data.NodeLabel
import           Data.Vector               (Vector)

import Debug.Trace

-- |
-- Remove all scoring data from nodes.
wipeScoring
  :: ( Default n
     , Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     , Show e
     )
  => PhylogeneticDAG2 m a d e n u v w x y z
  -> PhylogeneticDAG2 m a d e n (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
wipeScoring (PDAG2 dag m) = PDAG2 wipedDAG m
  where
    wipedDAG =
        RefDAG
          <$> fmap wipeDecorations . references
          <*> rootRefs
          <*> ((mempty, mempty, Nothing) <$) . graphData
          $ dag

    wipeDecorations
      :: (Default n
     , Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     , Show e
     )      => IndexData e (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
      -> IndexData e (PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n)
    wipeDecorations x = {- (\y -> trace ("\n\nOutput from wipeDecorations:\n\n" <> show y) y) $ -}
          IndexData
            <$> wipeNode shouldWipe . nodeDecoration
            <*> parentRefs
            <*> childRefs
            $ x
      where
        shouldWipe = not . null $ childRefs x


-- |
-- Conditionally wipe the scoring of a single node.
wipeNode
  :: ( Default n
     , Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     )
  => Bool -- ^ Do I wipe?
  -> PhylogeneticNode2 (CharacterSequence        u         v         w         x         y         z ) n
  -> PhylogeneticNode2 (CharacterSequence (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)) n
--wipeNode _ node | trace ("!!! wipeNode InputNode:\n" <> show node) False = undefined
wipeNode wipe = {-- (\x -> trace ("!!! wipeNode OutputNode:\n" <> show x) x) $ --} PNode2 <$> pure . g . NE.head . resolutions <*> f . nodeDecorationDatum2
      where
        f :: Default a => a -> a
        f | wipe      = const def
          | otherwise = id

        g = ResInfo
              <$> totalSubtreeCost
              <*> localSequenceCost
              <*> leafSetRepresentation
              <*> subtreeRepresentation
              <*> subtreeEdgeSet
              <*> topologyRepresentation
              <*> hexmap h h h h h h . characterSequence
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
  :: ( DiscreteCharacterDecoration v StaticCharacter
     , DiscreteCharacterDecoration x StaticCharacter
     , DiscreteCharacterDecoration y StaticCharacter
     , RangedCharacterDecoration u ContinuousChar
     , RangedCharacterDecoration w StaticCharacter
     , SimpleDynamicDecoration z DynamicChar
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     , Show m
     )
  => PhylogeneticDAG2 m StaticCharacter (Element DynamicChar) EdgeLength NodeLabel (Maybe u) (Maybe v) (Maybe w) (Maybe x) (Maybe y) (Maybe z)
  -> FinalDecorationDAG
performDecoration x = performPreOrderDecoration performPostOrderDecoration
  where
    performPreOrderDecoration :: PostOrderDecorationDAG (TraversalTopology, Double, Double, Double, Data.Vector.Vector (NE.NonEmpty TraversalFocusEdge)) -> FinalDecorationDAG
    performPreOrderDecoration =
        preorderFromRooting''
          adaptiveDirectOptimizationPreOrder
          edgeCostMapping
          contextualNodeDatum
          minBlockConext

        . preorderSequence''
          (const additivePreOrder)
          (const fitchPreOrder   )
          (const additivePreOrder)
          (const sankoffPreOrder )
          (const sankoffPreOrder )
          (const id2             )
      where
        adaptiveDirectOptimizationPreOrder meta dec kidDecs = directOptimizationPreOrder pairwiseAlignmentFunction meta dec kidDecs
          where
            pairwiseAlignmentFunction = selectDynamicMetric meta

    performPostOrderDecoration :: PostOrderDecorationDAG (TraversalTopology, Double, Double, Double, Data.Vector.Vector (NE.NonEmpty TraversalFocusEdge))
    performPostOrderDecoration = postOrderResult

    (minBlockConext, postOrderResult) = assignPunitiveNetworkEdgeCost post
    (post, edgeCostMapping, contextualNodeDatum) =
         assignOptimalDynamicCharacterRootEdges adaptiveDirectOptimizationPostOrder
         . traceShowId
         . postorderSequence'
             (const (g additivePostOrder))
             (const (g    fitchPostOrder))
             (const (g additivePostOrder))
             (g . sankoffPostOrder)
             (g . sankoffPostOrder)
             (g . adaptiveDirectOptimizationPostOrder)
         . (\e@(PDAG2 x _) -> trace (show . references $ x) e)
         . trace "\n\nSTART POST ORDER:\n\n"
         $ x

    g _  Nothing  [] = error "Uninitialized leaf node. This is bad!"
    g h (Just  v) [] = h v []
    g h        e  xs = h (error $ mconcat [ "We shouldn't be using this value.", show e, show $ length xs ]) xs

    adaptiveDirectOptimizationPostOrder meta = directOptimizationPostOrder pairwiseAlignmentFunction
      where
        pairwiseAlignmentFunction = selectDynamicMetric meta

{-
instance HasCharacterCost (Maybe u) Double where

    characterCost = lens (const 0) undefined


instance HasCharacterCost (Maybe u) Word where

    characterCost = lens (const 0) undefined
-}

-- |
-- An identity function which ignores the second parameter.
id2 :: a -> b -> a
id2 = const
