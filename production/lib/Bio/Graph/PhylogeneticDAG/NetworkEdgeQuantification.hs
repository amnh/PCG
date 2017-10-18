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
import           Data.Bits
import           Data.EdgeSet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
--import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Ord
import           Numeric.Extended.Real
import           Prelude            hiding (zipWith)

import Debug.Trace


-- |
-- Calculate and assign the punitive network edge cost for the DAG.
--
-- Consider a network \(N = (V, E)\), as commonly defined with an edge set \(E\)
-- and vertex set \(V\). Let this network be a directed, acyclic graph (DAG) with
-- a single root. Furthermore, consider the set of display trees \(T\), with
-- individual display trees denoted as \(τ \in T\), derived from the resolutions
-- of network edges in \(E\) with \(n\) leaf taxa. For a set of \(k\) character
-- blocks \(C = \(C_1, ... , C_k\)\), there is at least one most parsimonious
-- display tree \(τ_{min} \in T\) with a cost of \(cost \left(τ_{min} \right)\)
-- and with edge set \(E_{min} \subseteq E\) and vertex set
-- \(V_{min} \subseteq V\). We further denote the display tree with minimum cost
-- \(c_i\) for the character block \(C_i \in C\) as \(τ_i \in T\) with the
-- corresponding edge set \(E_i \subseteq E\) and vertex set \(V_i \subseteq V\).
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
-- For more detail on the quantification of a network with a single root, see:
--
-- /Phylogenetic network analysis as a parsimony optimization problem/, Wheeler 2015
--
-- We must also consider the cases of multi-rooted DAGs, extending the method
-- described above. Unlike in the single rooted DAG context where we find a
-- minimum display tree over all character blocks, in a multi-rooted DAG we find
-- the "minimal display forest" over all character blocks. In the multi-rooted
-- DAG, let \( \left\{r_1, r_2, ... r_k \right\} = \mathcal{R}\) be the set of
-- \(k\) set roots.
--
-- The minimal display forest is the set
--
-- 
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
        

-- BEGIN NEW


-- |
-- There is at least one most parsimonious (for all characters combined) display
-- forest. This display forest is analogous to the most parsimonious display tree
-- \(τ_min\) described in /Wheeler 2015/, with the following additional
-- constraints:
--
-- * For each root node in the input DAG, there is a corresponding display tree
--   with that root.
--
-- * Each diplay tree in the display forest is not connected to anothe display
--   tree. Equivelently, no two roots on the input DAG are connected.
--
-- * Each taxa in the input DAG is connected to exactly one root.
--
-- The most parsimonious display forest is the set of display trees which has
-- the minimal cost over all character blocks when adding together the cost of
-- each display tree *and* adding the root cost for each character block.

extractMostParsimoniusDisplayForest
  :: ( Foldable1 f
     , HasBlockCost u v w x y z Word Double
     , HasRootCost  u v w x y z      Double 
     )
  => f (ResolutionCache (CharacterSequence u v w x y z))
  -> ResolutionCache (CharacterSequence u v w x y z)
extractMostParsimoniusDisplayForest displayForests = NE.head minDisplayForests
  where
    minDisplayForests = NE.fromList $ minimaBy (comparing displayForestCost) displayForests

    displayForestCost dis = sum $ displayTreeCost <$> dis
      where
        displayTreeCost x = totalSubtreeCost x + sequenceRootCost rootCount (characterSequence x)
        rootCount         = length dis


-- |
-- /O(r * 2^d)/ where /r/ is the number of roots and /d/ is the number of network
-- nodes in the DAG.
-- 
-- Gathers the vlid display forests present in the input DAG.
--
-- A valid display forest satifies the following constraints:
--
-- * For each root node in the input DAG, there is a corresponding display tree
--   with that root.
--
-- * Each diplay tree in the display forest is not connected to anothe display
--   tree. Equivelently, no two roots on the input DAG are connected.
--
-- * Each taxa in the input DAG is connected to exactly one root.
--
gatherDisplayForests :: PhylogeneticDAG2 e n u v w x y z -> [ResolutionCache (CharacterSequence u v w x y z)]
gatherDisplayForests (PDAG2 dag) = result
  where
    -- First we collect all resolutions for each root node
    rootResolutions = resolutions . nodeDecoration . (refs !) <$> rootRefs dag
    refs = references dag

    -- Second we collect the valid root combinations.
    result = filterResolutionCombinationsBy formsValidDisplayForest rootResolutions


-- |
-- Takes the non-empty set of valid display forests and returns the display
-- forest that is minimal for each character block. 
extractMinimalDisplayTreePerBlock
  :: Foldable1 f
  => f (ResolutionCache (CharacterSequence u v w x y z)) -- ^ Set of valid display forests
  -> NonEmpty (ResolutionCache (CharacterSequence u v w x y z))  -- ^ Valid display forest for each character block
extractMinimalDisplayTreePerBlock = undefined
    

-- END NEW






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


-- |
-- /O( n * m )/
--
filterResolutionCombinationsBy
  :: ( Applicative f
     , Foldable f
     , Traversable t
     )
  => (t a -> Bool)
  -> t (f a)
  -> [t a]
filterResolutionCombinationsBy predicate = filter predicate . toList . sequenceA


formsValidDisplayForest :: Foldable1 f => f (ResolutionInformation s) -> Bool
formsValidDisplayForest xs = notContradictory && completeLeafCoverage
  where
    notContradictory     = transitivePropertyHolds resolutionsDoNotOverlap xs
    completeLeafCoverage = let bitVal = foldMap1 leafSetRepresentation xs
                           in  complement (bitVal `xor` bitVal) == bitVal
