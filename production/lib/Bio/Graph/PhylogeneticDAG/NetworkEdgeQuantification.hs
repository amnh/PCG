------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification where

import           Bio.Sequence
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Arrow            ((&&&))
import           Data.EdgeSet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
--import           Data.Semigroup
import           Data.Ord
import           Numeric.Extended.Real
import           Prelude            hiding (zipWith)

import Debug.Trace


-- |
-- Calculate and assign the punitive networkedge cost for the DAG.
--
-- Consider a network \(N = (V, E)\), as commonly defined with an edge set \(E\)
-- and vertex set \(V\).  Furthermore, consider the set of display trees \(T\),
-- with individual display trees denoted as \(τ \in T\), derived from the
-- resolutions of network edges in \(E\) with \(n\) leaf taxa. For a set of \(k\)
-- characters blocks \(C = \(C_1, ... , C_k\)\), there is at least one most
-- parsimonious display tree \(τ_{min} \in T\) with a cost of
-- \(cost \left(τ_{min} \right)\) and with edge set \(E_{min} \subseteq E\) and
-- vertex set \(V_{min} \subseteq V\). We further denote the display tree with
-- minimum cost \(c_i\) for the character block \(C_i \in C\) as \(τ_i \in T\) with
-- the corresponding edge set \(E_i \subseteq E\) and vertex set \(V_i \subseteq V\).
--
-- The network edge punative cost is \(\infty\) if there is an "unused" network
-- edge in the DAG. There exists an "unused" edge in the DAG if and only if there
-- exists a network edge \(e \in E\) such that:
--
-- \[ \forall C_i \in C \quad  τ_i \in T \; \text{is the minimal display tree for} \; C_i \text{and} \; e \not \in τ_i \]
--
-- If there does not exist such a network edge we consider all the network edges
-- in the DAG "used" and the punative network edge cost is defined as follows:
--
-- \[ \frac {\Sigma_{1}^{k} \; c_i \times | E_i \setminus E_{min} |} {2 \times \left( 2n -2 \right)} \]
--
-- This function performs this punative network edge cost calculation and updates
-- the DAG metadata to reflect the cost of the network context. 
assignPunitiveNetworkEdgeCost :: HasBlockCost u v w x y z i r => PhylogeneticDAG2 e n u v w x y z -> PhylogeneticDAG2 e n u v w x y z
assignPunitiveNetworkEdgeCost input@(PDAG2 dag) = PDAG2 $ dag { graphData = newGraphData }
  where
    punativeCost  = calculatePunitiveNetworkEdgeCost input
--    sequenceCosts = minimum . fmap totalSubtreeCost . resolutions . nodeDecoration . (references dag !) <$> rootRefs dag
    sequenceCosts = sum . fmap minimum . NE.transpose . fmap (fmap blockCost . toBlocks . characterSequence)
                  . (\x -> trace (renderResolutionContexts x) x)
                  . resolutions . nodeDecoration . (references dag !) <$> rootRefs dag
    newGraphData  =
        GraphData        
        { dagCost           = punativeCost + realToFrac (sum sequenceCosts)
        , networkEdgeCost   = punativeCost
        , rootSequenceCosts = realToFrac <$> sequenceCosts
        , graphMetadata     = graphMetadata $ graphData dag
        }
    renderResolutionContexts = unlines . fmap renderContext . toList 
      where
        renderContext x = unwords
            [ show $ totalSubtreeCost x
            , show $ subtreeEdgeSet x
            ]
        

-- |
-- Calculate the punitive networkedge cost for the DAG.
calculatePunitiveNetworkEdgeCost :: HasBlockCost u v w x y z i r => PhylogeneticDAG2 e n u v w x y z -> ExtendedReal
calculatePunitiveNetworkEdgeCost inputDag
  | cardinality extraneousEdges > 0 = -- trace ("Extraneous edges: " <> show extraneousEdges)
                                    -- . trace ("Entire     edges: " <> show entireNetworkEdgeSet)
                                    -- . trace ("Minimal Block edges: " <> show ((\(_,_,x) -> collapseToEdgeSet x) <$> minimalBlockNetworkDisplay)) $
                                      infinity
  | otherwise                       = realToFrac numerator / realToFrac denominator
  where
    extraneousEdges        = entireNetworkEdgeSet `difference` minimalRequiredEdgeSet 
    minimalRequiredEdgeSet = foldMap (\(_,_,x) -> collapseToEdgeSet x) minimalBlockNetworkDisplay
    entireNetworkEdgeSet   = extractNetworkEdgeSet inputDag

    numerator   = punativeEdgeCost minResult
    denominator :: Word
    denominator = cardinality minimalTotalNetworkDisplay --WLOG, all should be the same number of edges
    
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



-- |
-- There is at least one most parsimonious (for all characters combined) display tree τ min min with edge set E min and vertex set V min .
extractMostParsimoniusDisplayTree = undefined


-- |
-- Construct each most parsimonious display forest resolution with respect to the
-- DAG rootings.
extractNetworkMinimalDisplayTrees :: PhylogeneticDAG2 e n u v w x y z -> NonEmpty (NetworkDisplayEdgeSet (Int, Int))
extractNetworkMinimalDisplayTrees (PDAG2 dag) = rootTransformation rootResolutions
  where
    -- Since the number of roots in a DAG is fixed, each network display will
    -- contain an equal number of elements in the network display.
    rootTransformation = fmap (fromEdgeSets . NE.fromList . fmap subtreeEdgeSet)
                       . NE.fromList . minimaBy (comparing (sum . fmap totalSubtreeCost))
                       . pairwiseSequence resolutionsDoNotOverlap

    -- First we collect all resolutions for each root node
    rootResolutions = resolutions . nodeDecoration . (refs !) <$> rootRefs dag
    refs = references dag
    

-- |
-- Derive the entire edgeset of the DAG.
extractNetworkEdgeSet :: PhylogeneticDAG2 e n u v w x y z -> EdgeSet (Int, Int)
extractNetworkEdgeSet (PDAG2 dag) = getEdges dag


-- |
-- Construct a "Sequence" of minimal cost minimal and display tree resolutions
-- for each character block.
extractBlocksMinimalEdgeSets :: HasBlockCost u v w x y z i r 
                             => PhylogeneticDAG2 e n u v w x y z
                             -> NonEmpty (r, NonEmpty (NetworkDisplayEdgeSet (Int,Int)))
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

