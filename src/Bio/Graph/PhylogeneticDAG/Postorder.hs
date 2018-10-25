------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Postorder
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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Graph.PhylogeneticDAG.Postorder
  ( postorderSequence'
  ) where

import           Analysis.Parsimony.Internal
import           Bio.Character.Encodable
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.Arrow                      ((&&&))
import           Control.Lens.At                    (ix)
import           Control.Lens.Combinators           (singular)
import           Control.Lens.Operators             ((%~), (.~), (^.))
import           Data.Foldable.Custom               (foldMap', minimum', sum')
import           Data.Function                      ((&))
import qualified Data.IntMap                        as IM
import           Data.IntSet                        (IntSet)
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                 as NE
import           Data.MonoTraversable
import           Data.UnionSet                      (UnionSet)
import qualified Data.Vector                        as V


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /post-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
postorderSequence'
  :: forall m e n u v w x y z u' v' w' x' y' z' . HasBlockCost u' v' w' x' y' z'
  => (ContinuousCharacterMetadataDec                      -> PostorderContext u u' -> u')
  -> (DiscreteCharacterMetadataDec                        -> PostorderContext v v' -> v')
  -> (DiscreteCharacterMetadataDec                        -> PostorderContext w w' -> w')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> PostorderContext x x' -> x')
  -> (DiscreteWithTCMCharacterMetadataDec StaticCharacter -> PostorderContext y y' -> y')
  -> (DynamicCharacterMetadataDec (Element DynamicCharacter)   -> PostorderContext z z' -> z')
  -> PhylogeneticDAG2 m e n u  v  w  x  y  z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'

postorderSequence' f1 f2 f3 f4 f5 f6 pdag2@(PDAG2 dag m) = pdag2 & _phylogeneticForest .~ newRDAG
  where
    completeLeafSetForDAG :: UnionSet
    completeLeafSetForDAG = foldMap' f dag
      where
        f :: PhylogeneticNode2 charSeq decData -> UnionSet
        f = (^. _leafSetRepresentation) . NE.head . resolutions

    newRDAG =
      dag  & _graphData  %~ updateGraphCosts
           & _graphData  %~ setDefaultMetadata
           & _references .~ newReferences

    dagSize       = length $ references dag
    newReferences = memo

    updateGraphCosts :: GraphData d -> GraphData d
    updateGraphCosts g =
      g & _dagCost         .~ (realToFrac . sum' $ accessCost <$> rootRefs dag)
        & _networkEdgeCost .~ 0
        & _rootingCost     .~ 0
        & _totalBlockCost  .~ 0

      where
        accessCost :: Int -> Double
        accessCost = minimum'
                   . fmap (sequenceCost m . characterSequence)
                   . resolutions
                   . nodeDecoration
                   . (newReferences !)

    memo :: V.Vector (IndexData e (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n))
    memo = V.generate dagSize h
      where
        h i = (dag ^. _references . singular (ix i))
              & _nodeDecoration . _resolutions .~ newResolutions
          where
            newResolutions
              | i `notElem` rootRefs dag = localResolutions
              | otherwise =
                  case localResolutions of
                    x:|[] -> x:|[]
                    _ ->
                      case NE.filter completeCoverage localResolutions of
                        x:xs -> x:|xs
                        _    -> error "Root Node with no complete coverage resolutions!!! This should be logically impossible."
            completeCoverage :: ResolutionInformation s -> Bool
            completeCoverage = (completeLeafSetForDAG ==) . (^. _leafSetRepresentation)

            localResolutions
              = fmap (generateLocalResolutions f1 f2 f3 f4 f5 f6 m) childContextResolutions

            node :: IndexData e (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
            node             = references dag ! i

            childIndices :: ChildContext Int
            childIndices     = toChildContext . IM.keys . childRefs $ node

            datumResolutions :: NonEmpty (ResolutionInformation (CharacterSequence u v w x y z))
            datumResolutions = resolutions $ nodeDecoration node
            nodeResolution   = NE.head datumResolutions

            childContextResolutions
              :: NonEmpty
                   (ResolutionInformation
                     (PostorderContext
                       (CharacterSequence u  v  w  x  y  z )
                       (CharacterSequence u' v' w' x' y' z')
                     )
                   )
            childContextResolutions
              = applySoftwireResolutions nodeResolution
                  $ extractResolutionContext <$> childIndices

            extractResolutionContext
              :: Int
              -> (ResolutionCache (CharacterSequence u' v' w' x' y' z') , IntSet)
            extractResolutionContext = getResolutions &&& parentRefs . (references dag !)

            getResolutions
              :: Int
              -> NonEmpty
                   (ResolutionInformation (CharacterSequence u' v' w' x' y' z'))
            getResolutions j = fmap updateFunction . resolutions $ (memo ! j) ^. _nodeDecoration
              where
                updateFunction =
                    case otoList . parentRefs $ references dag ! j of
                      -- In the network edge case, we add also update the topology representation
                      x:y:_ ->
                          let  mutuallyExclusiveIncidentEdge = if x == i then (y,j) else (x,j)
                          in   addEdgeToEdgeSet (i,j) . addNetworkEdgeToTopology (i,j) mutuallyExclusiveIncidentEdge
                      _     -> addEdgeToEdgeSet (i,j)
