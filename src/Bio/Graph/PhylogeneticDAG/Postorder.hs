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

{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Bio.Graph.PhylogeneticDAG.Postorder
  ( postorderSequence'
  ) where

import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Control.Arrow               ((&&&))
import           Control.Applicative         (liftA2)
import           Data.Bits
import           Data.Foldable
import qualified Data.IntMap          as IM
import           Data.Key
import           Data.List.NonEmpty          (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty   as NE
import           Data.MonoTraversable
import qualified Data.Vector          as V


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
postorderSequence' :: HasBlockCost u' v' w' x' y' z' Word Double
                   => (u -> [u'] -> u')
                   -> (v -> [v'] -> v')
                   -> (w -> [w'] -> w')
                   -> (x -> [x'] -> x')
                   -> (y -> [y'] -> y')
                   -> (z -> [z'] -> z')
                   -> PhylogeneticDAG2 m a d e n u  v  w  x  y  z
                   -> PhylogeneticDAG2 m a d e n u' v' w' x' y' z'
postorderSequence' f1 f2 f3 f4 f5 f6 (PDAG2 dag m) = PDAG2 (newDAG dag) m
  where
    completeLeafSetForDAG = foldl' f zeroBits dag
      where
        f acc = (acc .|.) . leafSetRepresentation . NE.head . resolutions
    
{-    
    newDAG
      :: ReferenceDAG d e x
      -> ReferenceDAG
           d
           e
           (PhylogeneticNode2 (CharacterSequence u' v' w' x' y' z') n)
-}
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> ((mempty, mempty, Nothing) <$) . graphData
    dagSize       = length $ references dag
    newReferences = V.generate dagSize h
      where
        h i = IndexData <$> const (memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

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
            localResolutions = liftA2 (generateLocalResolutions f1 f2 f3 f4 f5 f6) datumResolutions childResolutions
                
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
