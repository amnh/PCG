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

module Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
  ( assignPunitiveNetworkEdgeCost
  , extractMostParsimoniusDisplayForest
  , extractMinimalDisplayForestPerBlock
  , gatherDisplayForests
  ) where


import           Bio.Metadata.Dynamic     --(TraversalFoci, TraversalFocusEdge, TraversalTopology, traversalFoci)
import           Bio.Sequence
import qualified Bio.Sequence.Block as BLK
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Control.Lens
import           Data.Bits
--import           Data.EdgeSet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.List.Utility
import           Data.Maybe                (fromJust)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                  (difference)
import qualified Data.Set           as S
import           Data.TopologyRepresentation
import           Data.Ord
import           Data.Vector               (Vector)
import           Numeric.Extended.Real
import           Prelude            hiding (zipWith)

--import Debug.Trace


-- |
-- Calculate and assign the punitive network edge cost for the DAG.
--
-- Consider a network, \(N = (V, E)\), as commonly defined with an edge set \(E\)
-- and vertex set \(V\). Let this network be a directed, acyclic graph (DAG) with
-- a single root. Furthermore, consider the set of display trees, \(T\), with
-- individual display trees denoted as \(τ \in T\), derived from the resolutions
-- of network edges in \(E\) with \(n\) leaf taxa. For a set of \(k\) character
-- blocks \(C = \(C_1, ... , C_k\)\), there is at least one most-parsimonious
-- display tree \(τ_{min} \in T\) with a cost of \(cost \left(τ_{min} \right)\)
-- and with edge set \(E_{min} \subseteq E\) and vertex set
-- \(V_{min} \subseteq V\). We further denote the display tree with minimum cost
-- \(c_i\) for the character block \(C_i \in C\) as \(τ_i \in T\) with the
-- corresponding edge set \(E_i \subseteq E\) and vertex set \(V_i \subseteq V\).
--
-- The network edge punitive cost is \(\infty\) if there is an "unused" network
-- edge in the DAG. There exists an "unused" edge in the DAG if and only if there
-- exists a network edge \(e \in E\) such that:
--
-- \[ \forall C_i \in C \quad  τ_i \in T \; \text{is the minimal display tree for} \; C_i \text{and} \; e \not \in τ_i \]
--
-- If there does not exist such a network edge we consider all the network edges
-- in the DAG "used" and the punitive network edge cost is defined as follows:
--
-- \[ \frac {\Sigma_{1}^{k} \; c_i \times | E_i \setminus E_{min} |} {2 \times \left( 2n -2 \right)} \]
--
-- For more details on the quantification of a network with a single root, see:
--
-- /Phylogenetic network analysis as a parsimony optimization problem/, Wheeler 2015
--
-- We must also consider the cases of multi-rooted DAGs, extending the method
-- described above. Unlike in the single-rooted DAG context where we find a
-- minimum display tree over all character blocks, in a multi-rooted DAG we find
-- the "minimal display forest" over all character blocks. In the multi-rooted
-- DAG, let \( \left\{r_1, r_2, ... r_k \right\} = \mathcal{R}\) be the set of
-- \(k\) set roots.
--
-- The minimal display forest is the set ??
--
--
--
-- This function performs this punitive network edge cost calculation and updates
-- the DAG metadata to reflect the cost of the network context.
assignPunitiveNetworkEdgeCost
  :: ( HasBlockCost u v w x y z i r
     , HasRootCost  u v w x y z   r
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => PhylogeneticDAG2 e n u v w x y z
  -> (NonEmpty (TraversalTopology, r, r, r, Vector (NonEmpty TraversalFocusEdge)), PhylogeneticDAG2 e n u v w x y z)
assignPunitiveNetworkEdgeCost input@(PDAG2 dag) = (outputContext, PDAG2 $ dag { graphData = newGraphData })
  where
    -- First grab all the valid display forests present in the DAG.
    displayForests =
        case gatherDisplayForests input of
          []   -> error "A very interesting error has occurred, There are not valid display forests in the DAG!"
          x:xs -> x:|xs

    -- Use the valid display forests to determine:
    --
    --  * The most-parsimonious display forest for /all/ characters.
    --
    --  * The most-parsimonious display forest for /each/ characters.
    --
    mostParsimoniousDisplayForest       = extractMostParsimoniusDisplayForest displayForests
    minimalDisplayForestPerBlockContext = extractMinimalDisplayForestPerBlock displayForests
    minimalDisplayForestPerBlock        = ignoreDynamicCharacterTraversalFoci minimalDisplayForestPerBlockContext

    -- We need the collection of network edges in the DAG to calculate the
    -- numerator of the network edge cost.
    networkEdgeSet = referenceNetworkEdgeSet dag

    -- We also need the size fo the edge set of the DAG to calculate the
    -- denominator of the network edge cost.
    edgeSetSize = toEnum . length $ referenceEdgeSet dag

    -- With all the requisite information in scope we can now compute the
    -- punitive network edge cost for the DAG.
    (punitiveCost, splitPunativeCosts) =
        calculatePunitiveNetworkEdgeCost
          edgeSetSize
          networkEdgeSet
          mostParsimoniousDisplayForest
          minimalDisplayForestPerBlock

    -- We also accumulate the cost of all the character blocks accros the display forests.
    cumulativeCharacterCost = sum $ (\(_,_,c) -> c) <$> minimalDisplayForestPerBlock

    -- And accumulate the root cost of all the blocks accross the display forests.
    cumulativeRootCost      = sum $ (\(_,c,_) -> c) <$> minimalDisplayForestPerBlock

    -- And lastly the total DAG cost
    totalCost = punitiveCost + realToFrac cumulativeCharacterCost + realToFrac cumulativeRootCost

    -- And store all the minimization work in a context to be returned.
    outputContext = zipWith f splitPunativeCosts minimalDisplayForestPerBlockContext
      where
        f x (a, b, c, d) = (a, b, x, c, d)

    newGraphData  =
        GraphData
        { dagCost           = totalCost
        , networkEdgeCost   = punitiveCost
        , rootingCost       = realToFrac cumulativeRootCost
        , totalBlockCost    = realToFrac cumulativeCharacterCost
        , graphMetadata     = graphMetadata $ graphData dag
        }



{-
assignPunitiveNetworkEdgeCost :: HasBlockCost u v w x y z i r => PhylogeneticDAG2 e n u v w x y z -> PhylogeneticDAG2 e n u v w x y z
assignPunitiveNetworkEdgeCost input@(PDAG2 dag) = PDAG2 $ dag { graphData = newGraphData }
  where
    punitiveCost  = calculatePunitiveNetworkEdgeCost input
--    sequenceCosts = minimum . fmap totalSubtreeCost . resolutions . nodeDecoration . (references dag !) <$> rootRefs dag
    sequenceCosts = sum . fmap minimum . NE.transpose . fmap (fmap blockCost . toBlocks . characterSequence)
                  . (\x -> trace (renderResolutionContexts x) x)
                  . resolutions . nodeDecoration . (references dag !) <$> rootRefs dag
    newGraphData  =
        GraphData
        { dagCost           = punitiveCost + realToFrac (sum sequenceCosts)
        , networkEdgeCost   = punitiveCost
        , rootSequenceCosts = realToFrac <$> sequenceCosts
        , graphMetadata     = graphMetadata $ graphData dag
        }
    renderResolutionContexts = unlines . fmap renderContext . toList
      where
        renderContext x = unwords
            [ show $ totalSubtreeCost x
            , show $ subtreeEdgeSet x
            ]
-}


-- |
-- /O(r * 2^d)/ where /r/ is the number of roots and /d/ is the number of network
-- nodes in the DAG.
--
-- Gathers the valid display forests present in the input DAG.
--
-- A valid display forest satisfies the following constraints:
--
-- * For each root node in the input DAG, there is a corresponding display tree
--   with that root.
--
-- * No display tree in the display forest is connected to another display
--   tree. Equivelently, no two roots on the input DAG are connected.
--
-- * Each taxon in the input DAG is connected to exactly one root.
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
-- There is at least one most-parsimonious (for all characters combined) display
-- forest. This display forest is analogous to the most-parsimonious display tree
-- \(τ_min\) described in /Wheeler 2015/, with additional constraints. See
-- 'gatherDisplayForests' for more information on the constraints of a valid
-- display forest.
--
-- The most-parsimonious display forest is the set of display trees which has
-- the minimal cost over all character blocks when adding together the cost of
-- each display tree *and* adding the root cost for each character block.
extractMostParsimoniusDisplayForest
  :: ( Foldable1 f
     , HasBlockCost u v w x y z i r
     , HasRootCost  u v w x y z   r
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => f (ResolutionCache (CharacterSequence u v w x y z))
  -> (TraversalTopology, r, r)
extractMostParsimoniusDisplayForest displayForests = (topo, rCost, bCost)
  where
    -- We select the most-parsimonious display forest.
    --
    -- If there are multiple equally-parsimonious display forests, then we
    -- select the first one.
    minDisplayForestWLOG  = head $ minimaBy (comparing displayForestCost) displayForests

    displayForestCost dis = sum $ displayTreeCost <$> dis
      where
        rootCount         = length dis
        displayTreeCost x = let charSeq = characterSequence x
                            in  sequenceCost charSeq + sequenceRootCost rootCount charSeq

    -- Once we have our most-parsimonious display forest we create the forest
    -- context for it.
    (topo, costs)  = createForestContext minDisplayForestWLOG

    -- Since we don't need the block-by-block contexts partitioned we accumulate
    -- the costs of the forest context.
    (rCost, bCost) = foldr1 (\(a,b) (c,d) -> (a+c, b+d)) $ (\(a,b,_) -> (a,b)) <$> costs


-- |
-- Takes the non-empty set of valid display forests and returns the display
-- forest that is minimal for each character block.
extractMinimalDisplayForestPerBlock
  :: ( Foldable1 f
     , Functor   f
     , HasBlockCost u v w x y z i r
     , HasRootCost  u v w x y z   r
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => f (ResolutionCache (CharacterSequence u v w x y z))                      -- ^ Set of valid display forests
  -> NonEmpty (TraversalTopology, r, r, Vector (NonEmpty TraversalFocusEdge)) -- ^ Valid display forest for each character block
                                                                              --   And the dynamic character rooting edges.
extractMinimalDisplayForestPerBlock displayForests = minimalBlockContexts
  where
    minimalBlockContexts = foldr1 (zipWith minimizeBlockContext) validBlockContexts
      where
        minimizeBlockContext lhs@(_, rCost1, bCost1, _) rhs@(_, rCost2, bCost2,_)
          | cost1 <= cost2 = lhs
          | otherwise      = rhs
          where
            cost1 = rCost1 + bCost1
            cost2 = rCost2 + bCost2

    validBlockContexts = linearizeContext . createForestContext <$> displayForests


-- |
-- Calculate the punitive network edge cost for the DAG.
calculatePunitiveNetworkEdgeCost
  :: ( Foldable f
--     , Num r
     , Floating r
     , Real r
     , Ord e
     )
  => Word                                      -- ^ Entire DAG edge-set cardinality
  -> f e                                       -- ^ Complete collection of network edges in the DAG
  -> (TopologyRepresentation e, r, r)          -- ^ Most-parsimonious display forest context
  -> NonEmpty (TopologyRepresentation e, r, r) -- ^ Minimal display forest context for each character block
  -> (ExtendedReal, NonEmpty r)
calculatePunitiveNetworkEdgeCost edgeSetCardinality networkEdgeSet parsimoniousContext minimalContexts
  | not (null extraneousEdges) = -- trace ("Extraneous edges: " <> show extraneousEdges)
                                 -- . trace ("Entire     edges: " <> show entireNetworkEdgeSet)
                                 -- . trace ("Minimal Block edges: " <> show ((\(_,_,x) -> collapseToEdgeSet x) <$> minimalBlockNetworkDisplay)) $
                                 (infinity, 0 <$ minimalContexts)
  | otherwise                  = (realToFrac $ sum punitiveCostPerBlock, punitiveCostPerBlock)
  where
    -- First we determine if there are extraneous network edges in the DAG.
    --
    -- We calculate the extraneous network edges by taking the difference between
    -- the complete netowrk edge set and the "used" network edge set.
    extraneousEdges   = totalNetworkEdges `difference` usedNetworkEdges
    totalNetworkEdges = S.fromList $ toList networkEdgeSet
    usedNetworkEdges  = foldMap1 (\(x,_,_) -> includedNetworkEdges x) minimalContexts

    -- Next we gather the set of network edges present in the most-parsimonious
    -- display forest.
    (parimoniousTopology,_,_) = parsimoniousContext
    mostParsimoniousNetworkEdgeSet = includedNetworkEdges parimoniousTopology

    -- We gather the punitive network edge cost for a block.
    punitiveEdgeCost (topo, _, costOfBlock) = costOfBlock * fromIntegral differingNetworkEdgeCount
      where
        differingNetworkEdgeCount = length differenceEdgeSet
        differenceEdgeSet         = minimalBlockEdgeSet `difference` mostParsimoniousNetworkEdgeSet
        minimalBlockEdgeSet       = includedNetworkEdges topo

    -- Then we apply the punitive edge cost to each character block.
    punitiveCostPerBlock = (/ divisor) . punitiveEdgeCost <$> minimalContexts
      where
        divisor = realToFrac $ 2 * edgeSetCardinality

    -- The numerator is the sum of the punitive network edge cost for each block.
--    numerator   = sum $ punitiveEdgeCost <$> minimalContexts
--    denominator = 2 * edgeSetCardinality


-- |
-- We create a forest context by accumulating the display tree topologies
-- and the cost for each block between roots.
--
-- Useful for comparing contexts when quantifying the punitive network edge cost.
createForestContext
  :: ( Foldable1 f
     , HasBlockCost u v w x y z i r
     , HasRootCost  u v w x y z   r
     , HasTraversalFoci z (Maybe TraversalFoci)
     )
  => f (ResolutionInformation (CharacterSequence u v w x y z))
  -> (TraversalTopology, NonEmpty (r, r, Vector (NonEmpty TraversalFocusEdge)))
createForestContext displayForest = fromBlockMinimizationContext $ foldMap1 blockContext displayForest
  where
    rootCount      = length displayForest
    getRootingEdge = fst . NE.head . fromJust . (^. traversalFoci)
    blockContext   = toBlockMinimizationContext <$> topologyRepresentation <*> blockCosts
      where
        blockCosts = fmap (\x -> (BLK.rootCost rootCount x, BLK.blockCost x, getRootingEdge <$> dynamicCharacters x)) . toBlocks . characterSequence


-- |
-- Take a display forest context and push the topology information through to
-- every block in the associated sequence. This creates a "linear" context
-- suitable for zipping.
linearizeContext :: (TraversalTopology, NonEmpty (a, b, c)) -> NonEmpty (TraversalTopology, a, b, c)
linearizeContext (topo, costs) = squashTopologyIntoContext <$> costs
  where
    squashTopologyIntoContext (x,y,z) = (topo, x, y, z)


ignoreDynamicCharacterTraversalFoci :: Functor f => f (a,b,c,d) -> f (a,b,c)
ignoreDynamicCharacterTraversalFoci = fmap (\(a,b,c,_) -> (a,b,c))




-- | Used for convient accumulation.
data BlockMinimizationContext c = BMC TraversalTopology (NonEmpty (c, c, Vector (NonEmpty TraversalFocusEdge)))
  deriving (Eq)


instance Num c => Semigroup (BlockMinimizationContext c) where

  (<>) (BMC topo1 costs1) (BMC topo2 costs2) = BMC (topo1 <> topo2) $ zipWith (\(a,b,c) (d,e,f) -> (a+d, b+e, zipWith (<>) c f)) costs1 costs2


toBlockMinimizationContext :: TraversalTopology -> NonEmpty (c, c, Vector TraversalFocusEdge) -> BlockMinimizationContext c
toBlockMinimizationContext x = BMC x . fmap (\(a,b,c) -> (a,b, pure <$> c))


fromBlockMinimizationContext :: BlockMinimizationContext c -> (TraversalTopology, NonEmpty (c, c, Vector (NonEmpty TraversalFocusEdge)))
fromBlockMinimizationContext (BMC topo costs) = (topo, costs)





{-
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

    numerator   = punitiveEdgeCost minResult
    denominator :: Word
    denominator = cardinality minimalTotalNetworkDisplay --WLOG, all should be the same number of edges

    minResult@(minimalTotalNetworkDisplay, minimalBlockNetworkDisplay) = minimumBy (comparing punitiveEdgeCost) minimalNetworkDisplaysWithMinimalBlocks

    minimalTotalNetworkDisplays = extractNetworkMinimalDisplayTrees inputDag
    minimalBlockNetworkDisplays = extractBlocksMinimalEdgeSets      inputDag

    minimalNetworkDisplaysWithMinimalBlocks = minimalBlocksForGivenNetworkEdgeDisplay <$> minimalTotalNetworkDisplays

    punitiveEdgeCost (_totalEdgeSet, blockEdgeSets) = sum $ fmap g blockEdgeSets
      where
        g (x,y,_) = x * fromIntegral y

    minimalBlocksForGivenNetworkEdgeDisplay displayEdgeSet = (displayEdgeSet, fmap gatherMinimalDisplayEdgeSetDiffernce minimalBlockNetworkDisplays)
      where
        gatherMinimalDisplayEdgeSetDiffernce (minBlockCost, minDisplayEdgeSets) = (minBlockCost, edgeDifference, minDifferenceDisplayEdgeSet)
          where
            (edgeDifference, minDifferenceDisplayEdgeSet) = minimumBy (comparing fst) $ (cardinality . (displayEdgeSet `difference`) &&& id) <$> minDisplayEdgeSets


-- |
-- Construct each most-parsimonious display forest resolution with respect to the
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
-}


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


-- |
-- A collection of resolutions, one resolution for each root in the DAG, forms a
-- valid display forest if the following constraints are satisfied:
--
-- * For each root node in the input DAG there is a corresponding display tree
--   with that root.
--
-- * No diplay tree in the display forest is connected to another display
--   tree. Equivelently, no two roots on the input DAG are connected.
--
-- * Each taxon in the input DAG is connected to exactly one root.
--
-- This function asserts that the conditions are met indirectly by using
-- invariants of the 'ResolutionInformation' construction in an earlier
-- computation.
formsValidDisplayForest :: Foldable1 f => f (ResolutionInformation s) -> Bool
formsValidDisplayForest resSet =
    case toNonEmpty resSet of
      _:|[] -> True
      _     -> notContradictory && completeLeafCoverage
  where
    notContradictory     = transitivePropertyHolds resolutionsDoNotOverlap resSet
    completeLeafCoverage = let bitVal = foldMap1 leafSetRepresentation resSet
                           in  complement (bitVal `xor` bitVal) == bitVal
