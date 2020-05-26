{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RecordWildCards     #-}

module Data.Graph.Postorder.NetworkEdgeQuantification where


import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Data.Graph.TopologyRepresentation
import Data.Graph.Type
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Graph.Postorder.Resolution (Resolution, HasLeafSetRepresentation(..))
import qualified Data.Graph.Postorder.Resolution as Resolution
import Data.Monoid
import Control.Lens
import Data.Bits
import Data.Graph.Sequence.Class
import Data.Graph.Postorder.DynamicTraversalFoci
import Data.Graph.Indices
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph.Postorder.Resolution

import Numeric.Extended.Real
import Data.Graph.NodeContext
import Data.Coerce
import Data.Foldable
import Data.List.Extra (minimumOn)
import Data.Pair.Strict
import Data.Semigroup.Foldable (foldMap1)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor


assignPunitiveNetworkEdgeCost
  ::
  ( HasMetadataSequence c (MetadataSequence block m)
  , HasBlockCost block
  )
  => Graph
       (ResolutionCacheM Identity) c e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> Vector (Vector (TraversalFoci))
  -> ( BlockMinimizationInfo
     , Graph
         (ResolutionCacheM Identity) c e
           (CharacterSequence block)
           (CharacterSequence (LeafBin block))         
     )
assignPunitiveNetworkEdgeCost inputGraph rootingTraversalFoci = undefined
  where
    meta = view (_cachedData . _metadataSequence) inputGraph
    displayForests =
      let
        displayForests = gatherDisplayForests inputGraph
      in
      case length displayForests of
        0 ->
          error $ fold
            [ "assingPunitiveNetworkEdgeCost:"
            , "There are not valid display forests in the DAG"
            ]
        1 -> displayForests


    forestContext :: Vector SequenceMinimizationInfo
    forestContext =
      fmap (createForestContext meta rootingTraversalFoci) displayForests


    mostParsimoniousDisplayForest =
      extractMostParsimonious meta rootingTraversalFoci displayForests

    

    minimalDisplayForestPerBlockContext
      = extractMinimalDisplayForest meta forestContext

    minimalDisplayForestBlockContext
      = extractMinimalDisplayForestPerBlock meta forestContext

    minimalDisplayForestPerBlock = undefined

    networkEdgeSet :: Set EdgeIndex
    networkEdgeSet = getNetworkEdgeSet inputGraph
    edgeSetSize = length networkEdgeSet

    punitiveCost :!: blockPunitiveCost =
      calculatePunitiveNetworkEdgeCost
        edgeSetSize
        networkEdgeSet
        mostParsimoniousDisplayForest
        minimalDisplayForestPerBlock

extractMinimalDisplayForest
  :: MetadataSequence block m
  -> Vector SequenceMinimizationInfo
  -> NetworkTopology :!: (Double :!: Double)
extractMinimalDisplayForest meta = undefined
  where
    minimalDisplayForest
      = undefined

    displayForestCost for = sum . fmap displayTreeCost $ for
      where
        rootCount = length for
        displayTreeCost =
          let
            charSeq = totalSequenceCost rootCount meta



extractMinimalDisplayForestPerBlock
  :: MetadataSequence block m
  -> Vector SequenceMinimizationInfo
  -> Vector BlockMinimizationInfo
extractMinimalDisplayForestPerBlock meta seqMinInfo = minimalBlockContexts
  where
    topology = undefined
    minimalBlockContexts
      = foldr1 (Vector.zipWith minimizeBlockContext) . fmap smiBlockMinimization $ seqMinInfo

    minimizeBlockContext lhs rhs
      | minimizationCost lhs <= minimizationCost rhs = lhs
      | otherwise                                    = rhs
   


gatherDisplayForests
  :: forall block c e . ()
  => Graph
       (ResolutionCacheM Identity) c e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> Vector (ResolutionCache (CharacterSequence block))
gatherDisplayForests graph = rootCombinations
  where
    rootResolutions :: Vector (ResolutionCache (CharacterSequence block))
    rootResolutions =
        fmap (view _nodeData)
      . view _rootReferences
      $ graph


    rootCombinations :: Vector (ResolutionCache (CharacterSequence block))
    rootCombinations =
      Vector.filter isValidDisplayForest rootResolutions


extractMostParsimonious
  :: forall block meta . (HasBlockCost block)
  => MetadataSequence block meta
  -> Vector (Vector TraversalFoci)
  -> Vector (ResolutionCache (CharacterSequence block))
  -> NetworkTopology :!: (Double :!: Double)
extractMostParsimonious meta rootingTraversalFoci displayForests =
    topology :!: minimalCosts
  where
    displayForestCost :: ResolutionCache (CharacterSequence block) -> Double
    displayForestCost dis =
      let
        resolutions = view _resolutionCache dis
        rootCount = length resolutions
        displayTreeCost res =
          let charSeq = view _characterSequence res in
            sequenceCost meta charSeq +
            sequenceRootCost rootCount meta charSeq
      in
        sum . (fmap displayTreeCost) $ resolutions

    minimumDisplayForest :: ResolutionCache (CharacterSequence block)
    minimumDisplayForest =
      minimumOn displayForestCost . Vector.toList $ displayForests

    sequenceMinimization = createForestContext meta rootingTraversalFoci minimumDisplayForest
    topology = smiTopology sequenceMinimization

    minimalCosts :: (Double :!: Double)
    minimalCosts =
        (\blockMin -> bmiRootCost blockMin :!: bmiBlockCost blockMin)
      . foldr1 (<>)
      . smiBlockMinimization
      $ sequenceMinimization
    


-- |
-- Calculate the punitive network edge cost for the DAG.
calculatePunitiveNetworkEdgeCost 
  :: Int                                     -- ^ Entire DAG edge-set cardinality
  -> Set EdgeIndex                           -- ^ Complete collection of network edges in the DAG
  -> NetworkTopology :!: (r :!: Double)      -- ^ Most-parsimonious display forest context
  -> Vector (NetworkTopology :!: (r :!: Double)) -- ^ Minimal display forest context for each character block
  -> ExtendedReal :!: Vector Double
calculatePunitiveNetworkEdgeCost
  edgeCardinality networkEdgeSet (bestTopology :!: (_ :!: _)) minimalContexts
  | not (null extraneousEdges) = infinity :!: (minimalContexts $> 0)
  | otherwise = realToFrac (sum punitiveCostPerBlock) :!: punitiveCostPerBlock

  where
 -- These are those edges in the DAG which are unused in any minimal context
 -- These are assigne an infinite cost.
    extraneousEdges = networkEdgeSet `Set.difference` usedNetworkEdges
    usedNetworkEdges =
      foldMap (\ (net :!: (_ :!: _)) -> includedNetworkEdges net) minimalContexts

    bestNetworkEdges = includedNetworkEdges bestTopology

 -- This function takes a display forest context and computes the blockCost
 -- where the cost is multiplied by the number of edges which do not appear
 -- in the most parsimonious network.
    punitiveEdgeCost :: NetworkTopology :!: (a :!: Double) -> Double
    punitiveEdgeCost (top :!:( _ :!: blockCost)) =
        blockCost * (fromIntegral mismatchCount)
      where
        mismatchCount = length networkEdgeMismatch
        networkEdgeMismatch =
          (includedNetworkEdges top) `Set.difference` bestNetworkEdges

 -- Finally we calculate the edge cost for each block dividing each by twice the total
 -- number of edges.
    punitiveCostPerBlock :: Vector Double
    punitiveCostPerBlock = fmap ((/ weight) . punitiveEdgeCost) minimalContexts
      where
        weight = realToFrac (2 * edgeCardinality)
    


createForestContext
  :: forall block m . (HasBlockCost block)
  => MetadataSequence block m
  -> Vector (Vector TraversalFoci)
  -> ResolutionCache (CharacterSequence block)
  -> SequenceMinimizationInfo
createForestContext metaSeq rootingTraversalFoci resCache =
    foldMap1 traversalEdgeCostInfo resolutions
  where
    resolutions = view _resolutionCache resCache
    rootCount = length resolutions
    unbiasedRootingEdge = pure . getUnbiasedRootEdge
    traversalEdgeCostInfo :: Resolution (CharacterSequence block) -> SequenceMinimizationInfo
    traversalEdgeCostInfo res =
      let
        topology = view _topologyRepresentation res
        metaBlocks = view _blockSequence metaSeq

        getBlockCost
          :: MetadataBlock block m -> Vector TraversalFoci -> block
          -> BlockMinimizationInfo
        getBlockCost metaBlock traversalFoci charBlock =
          BlockMinimizationInfo
            (rootCost rootCount metaBlock charBlock)
            (blockCost metaBlock charBlock)
            (unbiasedRootingEdge <$> traversalFoci)

        resCosts =
            SequenceMinimizationInfo topology
          . Vector.zipWith3 getBlockCost metaBlocks rootingTraversalFoci
          . view _blockSequence
          . view _characterSequence
          $ res
      in
        resCosts


data BlockMinimizationInfo = BlockMinimizationInfo
  { bmiRootCost       :: !Double
  , bmiBlockCost      :: !Double
  , bmiTraversalEdges :: Vector (NonEmpty EdgeIndex)
  }

minimizationCost :: BlockMinimizationInfo -> Double
minimizationCost BlockMinimizationInfo{..} =
  bmiRootCost + bmiBlockCost

instance Semigroup BlockMinimizationInfo where
  {-# INLINE (<>) #-}
  (<>) (BlockMinimizationInfo r1 b1 t1) (BlockMinimizationInfo r2 b2 t2)
    = BlockMinimizationInfo (r1 + r2) (b1 + b2) (Vector.zipWith (<>) t1 t2)



data SequenceMinimizationInfo = SequenceMinimizationInfo
  { smiTopology         :: !NetworkTopology
  , smiBlockMinimization :: Vector (BlockMinimizationInfo)
  }
  
instance Semigroup SequenceMinimizationInfo where
  {-# INLINE (<>) #-}
  (<>) (SequenceMinimizationInfo top1 c1) (SequenceMinimizationInfo top2 c2)
    = SequenceMinimizationInfo (top1 <> top2) (Vector.zipWith (<>) c1 c2)

-- |
-- This function takes a collection of resolutions from a root node and makes sure
-- that if there are more than one resolution then together the resolutions do
-- not overlap and that the resolutions include all leaves in the root subtree.
isValidDisplayForest ::  ResolutionCache cs -> Bool
isValidDisplayForest resCache =
  let
    resolutions = view _resolutionCache resCache
  in
  case resolutions of
    (r :| []) -> True
    (_ :| (_ : _)) ->
      let
        rs = NonEmpty.toList resolutions
        allDisjoint
          = getAll
          . foldMap (All . uncurry Resolution.disjointResolutions)
          $ zip rs (tail rs)
    
        completeCoverage =
          let bitVal = foldMap (view _leafSetRepresentation) resolutions
          in  complement (bitVal `xor` bitVal) == bitVal
      in
        completeCoverage && allDisjoint
  

