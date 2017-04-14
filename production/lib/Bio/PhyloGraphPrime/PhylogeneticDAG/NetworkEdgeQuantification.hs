------------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.PhylogeneticDAG.NetworkEdgeQuantification
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

module Bio.PhyloGraphPrime.PhylogeneticDAG.NetworkEdgeQuantification where

import           Bio.Character.Decoration.Shared
import           Bio.Metadata.General
import           Bio.Sequence
import           Bio.PhyloGraphPrime.EdgeSet
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.PhylogeneticDAG.Internal
import           Bio.PhyloGraphPrime.ReferenceDAG.Internal
import           Control.Arrow            ((&&&))
import           Data.ExtendedReal
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
import           Data.Ord
import           Prelude            hiding (zipWith)



calculatePunativeNetworkEdgeCost ::
    ( HasCharacterCost   u Word
    , HasCharacterCost   v Word
    , HasCharacterCost   w Double
    , HasCharacterCost   x Word
    , HasCharacterCost   y Word
    , HasCharacterCost   z Word
    , HasCharacterWeight u Double
    , HasCharacterWeight v Double
    , HasCharacterWeight w Double
    , HasCharacterWeight x Double
    , HasCharacterWeight y Double
    , HasCharacterWeight z Double
    ) => PhylogeneticDAG2 e n u v w x y z -> ExtendedReal
calculatePunativeNetworkEdgeCost inputDag
  | cardinality extraneousEdges > 0 = infinity
  | otherwise                       = realToFrac $ numerator / denominator
  where
    extraneousEdges        = entireNetworkEdgeSet `difference` minimalRequiredEdgeSet 
    minimalRequiredEdgeSet = foldMap (\(_,_,x) -> collapseToEdgeSet x) minimalBlockNetworkDisplay
    entireNetworkEdgeSet   = extractNetworkEdgeSet inputDag

    numerator       = punativeEdgeCost minResult
    denominator     = fromIntegral $ cardinality minimalTotalNetworkDisplay --WLOG, all should be the same number of edges
    
    minResult@(minimalTotalNetworkDisplay, minimalBlockNetworkDisplay) = minimumBy (comparing punativeEdgeCost) minimalNetworkDisplaysWithMinimalBlocks

    minimalTotalNetworkDisplays = extractNetworkMinimalDisplayTrees inputDag
    minimalBlockNetworkDisplays = extractBlocksMinimalEdgeSets      inputDag

    minimalNetworkDisplaysWithMinimalBlocks = minimalBlocksForGivenNetworkEdgeDisplay <$> minimalTotalNetworkDisplays

    punativeEdgeCost (_totalEdgeSet, blockEdgeSets) = sum $ fmap g blockEdgeSets 
      where
        g (x,y,_) = x * fromIntegral y

    minimalBlocksForGivenNetworkEdgeDisplay displayEdgeSet = (displayEdgeSet, fmap gatherMinimalDisplayEdgeSetDiffernce minimalBlockNetworkDisplays)
      where
        gatherMinimalDisplayEdgeSetDiffernce (minBlockCost, minDisplayEdgeSets) = (minBlockCost, edgeDifference, minDifferenceDisplayEdgeSet)
          where
            (edgeDifference, minDifferenceDisplayEdgeSet) = minimumBy (comparing fst) $ (cardinality . (displayEdgeSet `difference`) &&& id) <$> minDisplayEdgeSets


extractNetworkMinimalDisplayTrees :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty (NetworkDisplayEdgeSet (Int, Int))
extractNetworkMinimalDisplayTrees (PDAG2 dag) = rootTransformation rootResolutions
  where
    -- Since the number of roots in a DAG is fixed, deach network display will
    -- contain an equal number of elements in the network display.
    rootTransformation = fmap (fromEdgeSets . NE.fromList . fmap subtreeEdgeSet)
                       . NE.fromList . minimaBy (comparing (sum . fmap totalSubtreeCost))
                       . pairwiseSequence resolutionsDoNotOverlap

    -- First we collect all resolutions for each root node
    rootResolutions = resolutions . nodeDecoration . (refs !) <$> rootRefs dag
    refs = references dag
    

extractNetworkEdgeSet :: PhylogeneticDAG2 e n u v w x y z -> EdgeSet (Int, Int)
extractNetworkEdgeSet (PDAG2 dag) = getEdges dag


extractBlocksMinimalEdgeSets ::
  ( HasCharacterCost   u Word
  , HasCharacterCost   v Word
  , HasCharacterCost   w Double
  , HasCharacterCost   x Word
  , HasCharacterCost   y Word
  , HasCharacterCost   z Word
  , HasCharacterWeight u Double
  , HasCharacterWeight v Double
  , HasCharacterWeight w Double
  , HasCharacterWeight x Double
  , HasCharacterWeight y Double
  , HasCharacterWeight z Double
  ) => PhylogeneticDAG2 e n u v w x y z -> NonEmpty (Double, NonEmpty (NetworkDisplayEdgeSet (Int,Int)))
extractBlocksMinimalEdgeSets (PDAG2 dag) = foldMapWithKey1 f sequenceBlocksWLOG
  where
    generateBlockCosts = fmap (fmap (fmap (fmap blockCost . toBlocks)))
    rootingResolutions = generateBlockCosts $ pairwiseSequence resolutionsDoNotOverlap rootResolutions
    sequenceBlocksWLOG = toBlocks . characterSequence . NE.head $ NE.head rootResolutions
    rootResolutions = resolutions . nodeDecoration . (refs !) <$> rootRefs dag
    refs  = references dag

    focusOnBlockIndex i = (sum . fmap ((! i) . characterSequence)) &&& (fromEdgeSets . NE.fromList . fmap subtreeEdgeSet)

    f k _v =
        case minimaValues of
          x:xs -> pure (fst x, fmap snd $ x:|xs) 
          []   -> error ""
      where
        minimaValues = minimaBy (comparing fst) $ focusOnBlockIndex k <$> rootingResolutions
