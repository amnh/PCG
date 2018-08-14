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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Bio.Graph.PhylogeneticDAG.Postorder
  ( postorderSequence'
  ) where

import Bio.Character.Encodable
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Metadata.Continuous
import           Bio.Metadata.Discrete
import           Bio.Metadata.DiscreteWithTCM
import           Bio.Metadata.Dynamic
import           Bio.Sequence
import           Control.Applicative                (liftA2)
import           Control.Arrow                      ((&&&))
import           Data.Bits
import           Data.Foldable
import qualified Data.IntMap                        as IM
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                 as NE
import           Data.MonoTraversable
import qualified Data.Vector                        as V


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /post-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
postorderSequence'
  :: HasBlockCost u' v' w' x' y' z'
  => (ContinuousCharacterMetadataDec        -> u -> [u'] -> u')
  -> (DiscreteCharacterMetadataDec          -> v -> [v'] -> v')
  -> (DiscreteCharacterMetadataDec          -> w -> [w'] -> w')
  -> ((DiscreteWithTCMCharacterMetadataDec DiscreteCharacter)
      -> x -> [x'] -> x')
  -> ((DiscreteWithTCMCharacterMetadataDec DiscreteCharacter)
      -> y -> [y'] -> y')
  -> ((DynamicCharacterMetadataDec (Element DynamicChar)
      -> z -> [z'] -> z')
  -> PhylogeneticDAG2 m e n u  v  w  x  y  z
  -> PhylogeneticDAG2 m e n u' v' w' x' y' z'
--postorderSequence' f1 f2 f3 f4 f5 f6 (PDAG2 dag m) | (trace ((show . fmap length . otoList) m) False) = undefined
postorderSequence' f1 f2 f3 f4 f5 f6 (PDAG2 dag m) = PDAG2 (newDAG dag) m
  where
    completeLeafSetForDAG = foldl' f zeroBits dag
      where
        f acc = (acc .|.) . leafSetRepresentation . NE.head . resolutions

    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> ((mempty, mempty, Nothing) <$) . updateGraphCosts . graphData
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    updateGraphCosts g =
        GraphData
        { dagCost         = realToFrac . sum $ accessCost <$> rootRefs dag
        , networkEdgeCost = 0
        , rootingCost     = 0
        , totalBlockCost  = 0
        , graphMetadata   = graphMetadata g
        }
      where
        accessCost :: Int -> Double
        accessCost = minimum
                   . fmap (sequenceCost m . characterSequence)
                   . resolutions
                   . nodeDecoration
                   . (newReferences !)

--    memo :: Vector (PhylogeneticNode2 n (CharacterSequence u' v' w' x' y' z'))
    memo = V.generate dagSize h
      where
        h i =
          PNode2
              { resolutions          = newResolutions
              , nodeDecorationDatum2 = nodeDecorationDatum2 $ nodeDecoration node
              }
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

            completeCoverage = (completeLeafSetForDAG ==) . (completeLeafSetForDAG .&.) . leafSetRepresentation
            localResolutions = liftA2 (generateLocalResolutions f1 f2 f3 f4 f5 f6 m) datumResolutions childResolutions

            node             = references dag ! i
            childIndices     = IM.keys $ childRefs node
            datumResolutions = resolutions $ nodeDecoration node

--            childResolutions :: NonEmpty [a]
            childResolutions = applySoftwireResolutions $ extractResolutionContext <$> childIndices
            extractResolutionContext = getResolutions &&& parentRefs . (references dag !)
            getResolutions j = fmap updateFunction . resolutions $ memo ! j
              where
                updateFunction =
                    case otoList . parentRefs $ references dag ! j of
                      -- In the network edge case, we add also update the topology representation
                      x:y:_ ->
                          let  mutuallyExclusiveIncidentEdge = if x == i then (y,j) else (x,j)
                          in   addEdgeToEdgeSet (i,j) . addNetworkEdgeToTopology (i,j) mutuallyExclusiveIncidentEdge
                      _     -> addEdgeToEdgeSet (i,j)
