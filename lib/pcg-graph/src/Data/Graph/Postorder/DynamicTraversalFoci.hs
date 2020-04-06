------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
-- Copyright   :  (c) 2015-2020 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-- This module provides polymorphic traversal re-rooting for any type of
-- non-exact characters.
--
-- The top-level function `assignOptimalDynamicCharacterRootEdges` takes
-- a lens onto the relevant subBlocks of non-exact character elements, denoted
-- throughout by the polymorphic variable subBlock, and returns:
--
--   * A graph with costs updated to reflect the choice of traversal
--     edges for each non-exact character.
--
--   * A `HashMap` of type : HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
--     This gives a mapping from each edge in the graph to the resolutions we get
--     if we re-root and traverse from the chosen edge.
--
--   * A `HashMap` of type: HashMap NetworkTopology (Vector FinalBlockCostInfo)
--     This gives us a mapping from each display tree to the dynamic character
--     cost information for the chosen tree. The cost information includes
--     the total character cost for all non-exact characters along with the block-wise
--     and then character-wise cost for each character within a block and selection
--     of edges for which this minimal cost is attained.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}



module Data.Graph.Postorder.DynamicTraversalFoci
  ( assignOptimalDynamicCharacterRootEdges
  , FinalNonExactCostInfo(..)
  , FinalBlockCostInfo(..)
  )
  where

import           Control.Applicative
import           Control.Lens                      hiding (index)
import           Data.Coerce
import           Data.Foldable                     (fold, foldl')
import           Data.Graph.Indices
import           Data.Graph.Memo
import           Data.Graph.NodeContext
import           Data.Graph.Postorder.Resolution
import           Data.Graph.Sequence.Class
import           Data.Graph.TopologyRepresentation
import           Data.Graph.Type
import           Data.HashMap.Lazy                 (HashMap)
import qualified Data.HashMap.Lazy                 as HashMap
import           Data.Key
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Monoid                       (First (..), Sum (..))
import           Data.Pair.Strict
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Prelude                           hiding (lookup)

-- |
-- This function is the top-level function for performing re-rooting.
-- As it is polymorphic in the type of non-exact characters we are considering
-- one must provide lenses onto the relevant components.
assignOptimalDynamicCharacterRootEdges
  :: forall block subBlock dynChar m c e .
     ( BlockBin subBlock
     , HasSequenceCost block
     , HasBlockCost block
     , HasCharacterWeight (CharacterMetadata dynChar) Double
     , HasCharacterCost dynChar Word
     , DynCharacterSubBlock subBlock dynChar
     , HasMetadataSequence c (MetadataSequence block m)
     , Show block
     )
  => Lens' block (Vector dynChar)
  -> Lens' (LeafBin block) (LeafBin (Vector dynChar))
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block m
  -> Graph
       (ResolutionCacheM Identity) c e
       (CharacterSequence block) (CharacterSequence (LeafBin block))
  -> ( Graph
        (ResolutionCacheM Identity) c e
        (CharacterSequence block) (CharacterSequence (LeafBin block))
     , HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
     , HashMap NetworkTopology (Vector FinalBlockCostInfo)
     )
assignOptimalDynamicCharacterRootEdges _subBlock _subLeaf _subMeta meta graph =
  case numberOfNodes graph of
    0 -> (graph, mempty, mempty)
    1 -> (graph, mempty, mempty)
    2 ->
      let
     -- In this case we have a trivial solution consisting of
     -- a root node and leaf node and so we simply construct the
     -- construct the edge mapping.
        rootCache :: ResolutionCache (CharacterSequence block)
        rootCache  = view _nodeData . Vector.head $ view _rootReferences graph
        rootInd, childInd :: TaggedIndex
        rootInd  = TaggedIndex 0 RootTag
        childInd = TaggedIndex 0 LeafTag
        edge :: EdgeIndex
        edge = EdgeIndex rootInd childInd

        edgeMapping :: HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
        edgeMapping = HashMap.fromList [(edge, rootCache)]

        topologyMapping :: HashMap NetworkTopology (Vector FinalBlockCostInfo)
        topologyMapping = displayTreeRerooting _subBlock _subMeta meta edgeMapping

        updatedGraph = modifyRootCosts _subBlock topologyMapping graph
      in
        (updatedGraph, edgeMapping, topologyMapping)

    _ ->
      let
     -- This is a set of edges of the graph where we only take the directed
     -- edges from the original rooting (i.e. those from parent to children).
     -- We use an `Either` to indicate if an edge is a root index and if so
     -- we keep track of the root index in order to look up its resolutionCache.
        unrootedEdges :: Set (Either EdgeIndex (Int :!: EdgeIndex))
        unrootedEdges = getRootAdjacentEdgeSet graph <> getUnrootedEdges graph

     -- This is a mapping from edges to resolutions providing the resolutions
     -- we get if the edge is chosen as the traversal edge.

     -- Most of the actual work is done by contextNodeDatum which provides a memoized
     -- local resolution of each node arrangement (see memoizedEdgeMapping).

        contextNodeDatum :: GraphEdgeMapping block
        contextNodeDatum = memoizedEdgeMapping _subBlock _subLeaf _subMeta graph

        edgeMapping :: HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
        edgeMapping = HashMap.fromList . fmap f . Set.toList $ unrootedEdges
          where
          f :: Either EdgeIndex (Int :!: EdgeIndex)
            -> (EdgeIndex, ResolutionCache (CharacterSequence block))
          f = \case
            Right (rootInd :!: edge) ->
                (edge, view _nodeData (graph `indexRoot` rootInd))
            Left nonRootEdge@(EdgeIndex src tgt) ->
              let
                lhsContext
                  =   (src `lookupTreeNode` contextNodeDatum)
                  >>= (nonRootEdge `lookupEdge`)
                rhsContext
                  =   (tgt `lookupTreeNode` contextNodeDatum)
                  >>= (nonRootEdge `lookupEdge`)
                errorContext = error "to do"
--                      unlines
--                    [ "Could not find one or more of the contexts:"
--                    , unpack . renderDot $ toDot inputDag
--                    , ""
--                    , show inputDag
--                    , "Rooting Edge " <> show e
--                    , show $ HashMap.keys <$> contextNodeDatum
--                    ]
              in
                case liftA2 (,) lhsContext rhsContext of
                  Just (lhs, rhs) ->
                    let
                      leftChildResInfo, rightChildResInfo
                        :: (TaggedIndex, ResolutionCache (CharacterSequence block))
                      leftChildResInfo  = (src, lhs)
                      rightChildResInfo = (tgt, rhs)
                    in
                    ( nonRootEdge
                    , virtualParentResolution
                        _subBlock
                        _subMeta
                        meta
                        leftChildResInfo
                        rightChildResInfo
                    )
                  Nothing         -> error errorContext




        topologyMapping :: HashMap NetworkTopology (Vector FinalBlockCostInfo)
        topologyMapping = displayTreeRerooting _subBlock _subMeta meta edgeMapping

        updatedGraph = modifyRootCosts _subBlock topologyMapping graph
      in
        (updatedGraph, edgeMapping, topologyMapping)





--------------------------------------
-- Display tree / edge cost mapping --
--------------------------------------


-- |
-- This provides a memoized mapping wherein each internal node is assigned the edge
-- mapping. For more details see `deriveDirectedEdgeDatum`. Most of the work here
-- is just bookeeping the memoization.
memoizedEdgeMapping
  :: forall block subBlock dynChar m c e .
     ( BlockBin subBlock
     , HasSequenceCost block
     , HasBlockCost block
     , DynCharacterSubBlock subBlock dynChar
     , HasMetadataSequence c (MetadataSequence block m)
     , Show block
     )
  => Lens' block (Vector dynChar)
  -> Lens' (LeafBin block) (LeafBin (Vector dynChar))
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> Graph
       (ResolutionCacheM Identity) c e
       (CharacterSequence block) (CharacterSequence (LeafBin block))
  -> GraphEdgeMapping block
memoizedEdgeMapping _subBlock _subLeaf _subMeta graph =
  generateMemoGraphShape
    (length (view _leafReferences graph))
    (length (view _treeReferences graph))
    (length (view _networkReferences graph))
    (length (view _rootReferences graph))
    edgeMapMemo
  where
    edgeMapMemo :: Endo (GraphEdgeMemo block)
    edgeMapMemo graphEdgeMemo =
      let
        rootEdgeMap = const ()
        leafEdgeMap = const ()
        netEdgeMap  = internalMap NetworkTag
        treeEdgeMap = internalMap TreeTag

        internalMap tag n =
          let
            handleMaybe (Just v) = v
            handleMaybe Nothing  = error "Couldn't find node arrangement."

            tagInd = TaggedIndex n tag

            nodeArrangement  = getNodeArrangement tagInd graph
            nodeArrangements = allNodeArrangements (handleMaybe nodeArrangement)

          in
            deriveNodeArrangementEdgeMapping
              _subBlock _subLeaf _subMeta graph graphEdgeMemo tagInd nodeArrangements
      in
        MemoGen
          leafEdgeMap
          treeEdgeMap
          netEdgeMap
          rootEdgeMap

-- |
-- This function derives the resolutions for each of the edges in all permutations
-- of a node arrangement (i.e. considering each given each as a parent edge with the other
-- two child edges).
deriveNodeArrangementEdgeMapping
  :: forall block subBlock dynChar meta c e
  . ( HasMetadataSequence c (MetadataSequence block meta)
    , BlockBin block
    , BlockBin subBlock
    , HasSequenceCost block
    , DynCharacterSubBlock subBlock dynChar
    , Show block
    )
  => Lens' block subBlock
  -> Lens' (LeafBin block) (LeafBin subBlock)
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> Graph
       (ResolutionCacheM Identity)
       c e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> GraphEdgeMemo block
  -> TaggedIndex
  -> (NodeArrangement, NodeArrangement, NodeArrangement)
  -> EdgeMapping (ResolutionCache (CharacterSequence block))
deriveNodeArrangementEdgeMapping
  _subBlock _subLeaf _subMeta
  graph contextNodeDatum nodeInd (n1, n2, n3)
  =
  let
    deriveEdge = deriveDirectedEdgeDatum _subBlock _subLeaf _subMeta graph contextNodeDatum nodeInd
  in
    EdgeMapping
      (deriveEdge n1)
      (deriveEdge n2)
      (deriveEdge n3)

-- |
-- This function takes a node arrangement giving a selected node and those edges
-- surrounding it and returns the parent edge in the arrangement and the resolutions
-- when the given parent node is selected (see below).
--
-- In terms of the following diagram of a node arrangement:
--
--      (p)
--       |
--      (n)
--     /   \
--   (l)   (r)
--
-- we return the edge: Edge i n, along with the resolution cache
-- obtained if we treat nodes l and r as child nodes in the traversal
-- and perform a local postorder resolution update from the selected
-- (memoized) resolutions j and k.
--
-- Note: In terms of the original graph orientation given by the original
-- choice of rooting it may be the case that p is not the parent of n
-- but is only considered so in a given new choice of edge traversal.
deriveDirectedEdgeDatum
  :: forall block subBlock dynChar meta c e
  . ( HasMetadataSequence c (MetadataSequence block meta)
    , BlockBin block
    , BlockBin subBlock
    , HasSequenceCost block
    , DynCharacterSubBlock subBlock dynChar
    , Show block
    )
  => Lens' block subBlock
  -> Lens' (LeafBin block) (LeafBin subBlock)
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> Graph
       (ResolutionCacheM Identity)
       c e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> GraphEdgeMemo block
  -> TaggedIndex
  -> NodeArrangement
  -> DirEdgeIndex :!: ResolutionCache (CharacterSequence block)
deriveDirectedEdgeDatum
  _subBlock _subLeaf _subMeta
  graph contextNodeDatum nodeInd (NodeArrangement p l r) =
    p :!: subTreeResolutions
  where
    meta = view (_cachedData . _metadataSequence) graph
 -- Get the edge indices with their original graph orientation.
    leftGraphEdge  = dirToEdgeIndex l
    leftIndex      = view _edgeTarget leftGraphEdge
    rightGraphEdge = dirToEdgeIndex r
    rightIndex     = view _edgeTarget rightGraphEdge

    lhsMemo = contextNodeDatum `graphEdgeLookup` leftGraphEdge
    rhsMemo = contextNodeDatum `graphEdgeLookup` rightGraphEdge
    lhsContext = filterResolutionEdges (Set.singleton leftGraphEdge ) lhsMemo
    rhsContext = filterResolutionEdges (Set.singleton rightGraphEdge)  rhsMemo

    localResolution = virtualParentResolution _subBlock _subMeta meta
    toResCache (x : xs) = view (from _resolutionCache) (x :| xs)
    toResCache _ =
      error $
      fold
      [ "During deriveDirectedEdgeDatum "
      , "encoutered an empty resolution cache"
      ]

    leftChildResInfo  = (leftIndex  , lhsMemo)
    rightChildResInfo = (rightIndex , rhsMemo)
    emptyResolutionsError =
      error $ unlines
        [ "deriveDirectedEdgeDatum: Found two edges with empty resolutions:"
        , "lhsContext :"
        , "    " <> show leftChildResInfo
        , "rhsContext :"
        , "    " <> show rightChildResInfo
        ]
    subTreeResolutions
     -- In the case where the parent edge is not flipped
     -- then this is the orientation from the original graph
     -- and so we simply get the already computed resolution.
      | not . reversedEdge $ p = getResolutionCache meta nodeInd graph
      | otherwise =
        case (isUnrootedNetworkEdge l, isUnrootedNetworkEdge r) of
          (False, False) ->
            if not . isUnrootedNetworkEdge $ p
            then
              localResolution leftChildResInfo rightChildResInfo
            else
              case (lhsContext, rhsContext) of
                ([], []) -> emptyResolutionsError

                (xs, []) -> toResCache xs
                ([], ys) -> toResCache ys
                (xs, ys)
                  -> localResolution (leftIndex, toResCache xs) (rightIndex, toResCache ys)
          (False, True) ->
            case lhsContext of
              [] -> rhsMemo
              xs ->
                let
                  lhsMemo' = toResCache xs
                in
                  lhsMemo' <> localResolution (leftIndex, lhsMemo') (rightIndex, rhsMemo)

          (True, False) ->
            case rhsContext of
              [] -> lhsMemo
              xs ->
                let
                  rhsMemo' = toResCache xs
                in
                  rhsMemo' <> localResolution (leftIndex, lhsMemo) (rightIndex, rhsMemo')
          (True, True) ->
            case (lhsContext, rhsContext) of
              ([], []) -> emptyResolutionsError
              (xs, []) -> toResCache xs
              ([], ys) -> toResCache ys
              (xs, ys) -> toResCache (xs <> ys)


-- |
-- This function takes a mapping from edges to resolutions
-- and transposes it to be a map from those topologies which
-- are included in the resolutions to the list of edges which
-- use the topologies along with the character sequence of
-- that particular edge.
transposeDisplayTrees
  :: forall cs . ()
  => HashMap EdgeIndex (ResolutionCache cs)
  -> HashMap NetworkTopology (NonEmpty (EdgeIndex, cs))
transposeDisplayTrees =
    foldlWithKey' f mempty
  where
  f :: HashMap NetworkTopology (NonEmpty (EdgeIndex, cs))
    -> EdgeIndex
    -> ResolutionCache cs
    -> HashMap NetworkTopology (NonEmpty (EdgeIndex, cs))
  f outerMapRef rootingEdge = foldl' g outerMapRef . view _resolutionCache
    where
      g :: HashMap NetworkTopology (NonEmpty (EdgeIndex, cs))
        -> Resolution cs
        -> HashMap NetworkTopology (NonEmpty (EdgeIndex, cs))
      g innerMapRef resolution = HashMap.insertWith (<>) key val innerMapRef
        where
          key = view  _topologyRepresentation resolution
          val = pure (rootingEdge, view _characterSequence resolution)


------------------------------
-- Traversal Cost Functions --
------------------------------


-- |
-- This type represents the character cost information of traversing
-- a non-exact character from a particular edge.
data NonExactCharacterCostInfo = NonExactCharacterCostInfo
  { nonExactCharacterCost   :: !Word
  , nonExactCharacterWeight :: !Double
  , traversalEdgeFoci       :: NonEmpty EdgeIndex
  }

-- |
-- This type represents the final non-exact character cost
-- along with the minimal collection of traversal edges for this
-- particular type.
data FinalNonExactCostInfo = FinalNonExactCostInfo
  { finalCharacterCost :: !Word
  , minimalEdgeFoci    :: NonEmpty EdgeIndex
  }

-- |
-- Takes `NonExactCharacterCostInfo` which is minimal for this character
-- and extracts the non-exact character cost along with the traversal edge
-- assignment.
extractFinalCostInfo :: NonExactCharacterCostInfo -> FinalNonExactCostInfo
extractFinalCostInfo (NonExactCharacterCostInfo cost _ edgeFoci)
  = FinalNonExactCostInfo cost edgeFoci

-- |
-- This type collects together the static character costs for an entire block
-- along with a vector of `NonExactCharacterCostInfo` for each dynamic character
-- contained within a block.
data BlockCostInfo = BlockCostInfo
  { staticBlockCost       :: !Double
  , nonExactBlockCostInfo :: !(Vector NonExactCharacterCostInfo)
  }

-- |
-- This type represents a final assignment of cost and non-exact character
-- traversal information.
data FinalBlockCostInfo = FinalBlockCostInfo
  { totalCharacterCost    :: !Double
  , finalNonExactCostInfo :: !(Vector FinalNonExactCostInfo)
  }


-- |
-- This function takes a hashmap from edges to resolutions representing those
-- resolutions we get if this edge is chosen as a traversal edge and returns a
-- hashmap from network topologes to dynamic block cost information. This represents
-- the cost and traversal edges for each dynamic character block if this edge is
-- to be chosen.
displayTreeRerooting
    :: forall block subBlock dynChar m .
    ( HasCharacterCost dynChar Word
    , HasCharacterWeight (CharacterMetadata dynChar) Double
    , HasBlockCost block
    , DynCharacterSubBlock subBlock dynChar
    )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block m
  -> HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
  -> HashMap NetworkTopology (Vector FinalBlockCostInfo)
displayTreeRerooting _block _meta meta =
  fmap (deriveMinimalSequenceForDisplayTree _block _meta meta)  . transposeDisplayTrees


-- |
-- This function takes a non-empty collection of edges and character sequences.
-- The character sequence is that which is assigned to the edge if this is chosen
-- as a traversal edge. This function outputs a `FinalBlockCostInfo` for each
-- block in the character sequence. These give the minimal traversal edge costs
-- and the edges which are minimal for each block.
deriveMinimalSequenceForDisplayTree
  :: forall block subBlock dynChar m .
    ( HasCharacterCost dynChar Word
    , HasCharacterWeight (CharacterMetadata dynChar) Double
    , HasBlockCost block
    , DynCharacterSubBlock subBlock dynChar
    )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block m
  -> NonEmpty (EdgeIndex, CharacterSequence block)
  -> Vector FinalBlockCostInfo
deriveMinimalSequenceForDisplayTree _block _meta meta edgeMapping =
  let
    seqCost :: NonEmpty (Vector BlockCostInfo)
    seqCost = uncurry (getSequenceCostInfo _block _meta meta) <$> edgeMapping

    minimalBlocks :: Vector BlockCostInfo
    minimalBlocks = foldr1 (Vector.zipWith minimizeBlock) seqCost

    finalCostAssignment = fmap recomputeCost minimalBlocks
  in
    finalCostAssignment


-- |
-- Take two different `NonExactCharacterCostInfo` assignments and return
-- the assignment with minimal traversal cost. If both traversal costs
-- are equal then we merge the minimal traversal edge sets.
mergeNonExactCostInfo
  :: NonExactCharacterCostInfo
  -> NonExactCharacterCostInfo
  -> NonExactCharacterCostInfo
mergeNonExactCostInfo
  info1@(NonExactCharacterCostInfo traversalCost1 weight1 edgeFoci1)
  info2@(NonExactCharacterCostInfo traversalCost2 _       edgeFoci2) =
  case compare traversalCost1 traversalCost2 of
    LT -> info1
    GT -> info2
    EQ -> NonExactCharacterCostInfo traversalCost1 weight1 (edgeFoci1 <> edgeFoci2)


-- |
-- This function takes two `BlockCostInfo` assignments and merges the minimal
-- one character by character using `mergeNonExactCostInfo`.
minimizeBlock
  :: BlockCostInfo
  -> BlockCostInfo
  -> BlockCostInfo
minimizeBlock (BlockCostInfo static1 dyn1) (BlockCostInfo _ dyn2)
  = BlockCostInfo static1 (Vector.zipWith mergeNonExactCostInfo dyn1 dyn2)


-- |
-- This function take a character block containing dynamic characters
-- and a currently chosen edge and creates the block cost info associated
-- with traversing from that edge for that particular block.
getBlockCostInfo
  :: forall block subBlock dynChar m .
    ( HasCharacterCost dynChar Word
    , HasCharacterWeight (CharacterMetadata dynChar) Double
    , HasBlockCost block
    , DynCharacterSubBlock subBlock dynChar
    )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> EdgeIndex
  -> MetadataBlock block m
  -> block
  -> BlockCostInfo
getBlockCostInfo _subBlock _meta edge meta block = BlockCostInfo{..}
  where
    binMeta           = view _binMetadata meta
    dynBlock          = view _subBlock block
    dynBlockMeta      = view _meta binMeta

    getCharCostInfo charMeta dynChar
      = NonExactCharacterCostInfo
      { nonExactCharacterCost   = view _characterCost dynChar
      , nonExactCharacterWeight = view _characterWeight charMeta
      , traversalEdgeFoci       = pure edge
      }

    nonExactBlockCostInfo =
      Vector.zipWith getCharCostInfo dynBlockMeta dynBlock

    staticBlockCost   = staticCost meta block


-- |
-- This function takes a character sequence of blocks which contain
-- blocks of dynamic characters and returns the `BlockCostInfo` for each
-- block.
getSequenceCostInfo
  :: forall block subBlock dynChar m .
    ( HasCharacterCost dynChar Word
    , HasCharacterWeight (CharacterMetadata dynChar) Double
    , HasBlockCost block
    , DynCharacterSubBlock subBlock dynChar
    )
  => Lens' block subBlock
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block m
  -> EdgeIndex
  -> CharacterSequence block
  -> Vector BlockCostInfo
getSequenceCostInfo _block _meta metaSeq edge blockSeq =
    Vector.zipWith
      (getBlockCostInfo _block _meta edge)
      (view _blockSequence metaSeq)
      (view _blockSequence blockSeq)




-- |
-- This function gets the old static character cost
-- and the cost for each dynamic character with minimal
-- chosen traversal edges and gives back the final total
-- cost and dynamic character block information
recomputeCost :: BlockCostInfo -> FinalBlockCostInfo
recomputeCost BlockCostInfo{..} =
    FinalBlockCostInfo totalCost (extractFinalCostInfo <$> nonExactBlockCostInfo)
  where
    totalCost    = staticBlockCost + minDynCharCost

    getWeightedCost NonExactCharacterCostInfo{..}
      = Sum (fromIntegral nonExactCharacterCost * nonExactCharacterWeight)
    minDynCharCost
      = getSum . foldMap getWeightedCost $ nonExactBlockCostInfo


----------------------------
-- Graph update functions --
----------------------------


-- |
-- This function takes the topology mapping and uses it to figure out
-- the non-exact cost for each network topology in the resolutions
-- of root nodes by looking at the assigned costs that this topology
-- received when the optimal traversal edge is chosen.
modifyRootCosts
  :: forall c e block subBlock dynChar meta .
  ( DynCharacterSubBlock subBlock dynChar
  , HasCharacterCost dynChar Word
  , HasSequenceCost block
  , HasMetadataSequence c (MetadataSequence block meta)
  )
  => Lens' block subBlock
  -> HashMap NetworkTopology (Vector FinalBlockCostInfo)
  -> Graph
        (ResolutionCacheM Identity) c e
        (CharacterSequence block) (CharacterSequence (LeafBin block))
  -> Graph
        (ResolutionCacheM Identity) c e
        (CharacterSequence block) (CharacterSequence (LeafBin block))
modifyRootCosts _subBlock topologyMapping graph =
  let
    updateRoots =
      fmap $                -- over each root node
        over _nodeData $    -- over the index node data
          mapResolution     -- over the resolutionCache
          updateResolution  -- update the resolution

  in
    over _rootReferences updateRoots graph
  where
    meta = view (_cachedData . _metadataSequence) graph
    updateResolution :: Resolution (CharacterSequence block) -> Resolution (CharacterSequence block)
    updateResolution res =
        set _totalSubtreeCost  updatedTotalCost
      . set _characterSequence updatedCharSeq $ res
      where
        currCharSeq = view _characterSequence res
     -- Compute the cost of the updated sequence.
        updatedTotalCost = sequenceCost meta updatedCharSeq

     -- Here we take the block cost info and update the cost with the
     -- new traversal cost for each block.
        updatedCharSeq :: CharacterSequence block
        updatedCharSeq  =
          currCharSeq &  -- Treat the character sequence as a vector of blocks
            over _blockSequence
              (Vector.zipWith
                updateCharBlock
                finalBlockCostInfo)

     -- extract the topology at the root
        rootTopology = view _topologyRepresentation res
        finalBlockCostInfo = topologyMapping HashMap.! rootTopology


    updateCharBlock :: FinalBlockCostInfo -> block -> block
    updateCharBlock finalBlockCostInfo =
      over _subBlock (updateCharSubBlock finalBlockCostInfo)

    updateCharSubBlock :: FinalBlockCostInfo -> subBlock -> subBlock
    updateCharSubBlock (FinalBlockCostInfo _ dynBlockCostInfo) =
      Vector.zipWith set _characterCost finalCharacterCost dynBlockCostInfo



----------------------
-- Helper functions --
----------------------

-- |
-- This function removes those resolutions
-- which contain edges from the given set.
filterResolutionEdges
  :: Set EdgeIndex
  -> ResolutionCache (CharacterSequence block)
  -> [Resolution (CharacterSequence block)]
filterResolutionEdges edges = runIdentity . filterResolution hasEdge
  where
    hasEdge :: Resolution (CharacterSequence block) -> Bool
    hasEdge res =
      let
        resolutionEdges = view _subTreeEdgeSet res
      in
        edges `Set.disjoint` resolutionEdges

                        --------------------------------------
                        -- Edge Mapping and Node Arrangement--
                        --------------------------------------


-- |
-- This data type represents a selection of edges
-- surrounding a node in a re-rooted graph along with
-- data attached to each chosen edge/node as shown below:
--
--                      (p) - parVal
--                       |
--                      (n)
--                     /   \
--         leftVal - (l)   (r) - rightVal
--
data EdgeMapping val = EdgeMapping
  { parentEdge     :: DirEdgeIndex :!: val
  , leftChildEdge  :: DirEdgeIndex :!: val
  , rightChildEdge :: DirEdgeIndex :!: val
  }
  deriving stock (Show, Functor)



-- |
-- A `NodeArrangement` is an `EdgeMapping` with no (meaningful) data stored
-- in each node.
type NodeArrangement = EdgeMapping ()

{-# COMPLETE NodeArrangement #-}
pattern NodeArrangement :: DirEdgeIndex -> DirEdgeIndex -> DirEdgeIndex -> NodeArrangement
pattern NodeArrangement e1 e2 e3 <- EdgeMapping (e1 :!: ()) (e2 :!: ()) (e3 :!: ())  where
  NodeArrangement e1 e2 e3
    = EdgeMapping
        (e1 :!: ())
        (e2 :!: ())
        (e3 :!: ())


-- |
-- This function gets all of the node arrangements from a directed node arrangement:
--      (i)
--       |
--       v
--      (n)
--     /   \
--    V     V
--   (j)   (k)
--
-- For this purpose we keep track of the original relationships between the source and target.
--
allNodeArrangements :: NodeArrangement -> (NodeArrangement, NodeArrangement, NodeArrangement)
allNodeArrangements arrange@(NodeArrangement p l r)
  =  ( arrange
     , NodeArrangement (flipDirEdgeIndex l) r (flipDirEdgeIndex p)
     , NodeArrangement (flipDirEdgeIndex r) (flipDirEdgeIndex p) l
     )


-- |
-- Given an index this returns the node arrangement of that index. If not provided
-- with an internal node then this returns `Nothing`.
getNodeArrangement :: TaggedIndex -> Graph f c e n t -> Maybe NodeArrangement
getNodeArrangement tagInd graph =
  let
    untaggedInd = view _untaggedIndex tagInd
  in
  case view _indexType tagInd of
    LeafTag    -> Nothing
    RootTag    -> Nothing
    NetworkTag ->
      let
        nodeInfo = graph `indexNetwork` untaggedInd
        childInd             = view _childInds nodeInfo
        parInd1 :!: parInd2  = view _parentInds nodeInfo
        leftChildEdge
          = DirEdgeIndex (EdgeIndex tagInd parInd1) True
        rightChildEdge
          = DirEdgeIndex (EdgeIndex tagInd parInd2) True
        parentEdge
          = DirEdgeIndex (EdgeIndex tagInd childInd) True
      in
        Just $
          NodeArrangement parentEdge leftChildEdge rightChildEdge
    TreeTag    ->
      let
        nodeInfo = graph `indexTree` untaggedInd
        leftChild :!: rightChild = view _childInds nodeInfo
        parInd    = view _parentInds nodeInfo
        leftChildEdge
          = DirEdgeIndex (EdgeIndex tagInd leftChild) False
        rightChildEdge
          = DirEdgeIndex (EdgeIndex tagInd rightChild) False
        parentEdge
          = DirEdgeIndex (EdgeIndex tagInd parInd) False
      in
        Just $
          NodeArrangement parentEdge leftChildEdge rightChildEdge

-- |
-- This is a type that maps Tree and Network nodes to edge mappings
-- and leaf and root nodes to `()`. This has the same shape as the original
-- graph.
type GraphEdgeMapping block =
  GraphShape
    (EdgeMapping (ResolutionCache (CharacterSequence block)))
    (EdgeMapping (ResolutionCache (CharacterSequence block)))
    ()
    ()

-- |
-- This is a type for generating a memoized `GraphEdgeMapping`.
-- see `contextNodeDatum` for how this is used.
type GraphEdgeMemo block =
  MemoGen
    (EdgeMapping (ResolutionCache (CharacterSequence block)))
    (EdgeMapping (ResolutionCache (CharacterSequence block)))
    ()
    ()

-- |
-- Given an edge and an edge mapping we find the edge data associated to that edge.
-- We note that this ignores the directionality of the edge in the edge mapping.
lookupEdge :: forall edgeData . EdgeIndex -> EdgeMapping edgeData -> Maybe edgeData
lookupEdge edgeInd EdgeMapping{..} =
    getFirst . foldMap f $ [parentEdge, leftChildEdge, rightChildEdge]
  where
    f :: DirEdgeIndex :!: edgeData -> First edgeData
    f (dirEdgeIndex :!: edgeData)
      | edgeInd == view _edgeIndex dirEdgeIndex = pure edgeData
      | otherwise                                 = mempty

-- |
-- Given a memoized `GraphEdgeMapping` we look up a given edge returning
-- the resolutionCache. This function calls error if the given edge is not
-- found and so should only be used when this is satisfied.
--
-- This function also calls error if called with a edge from root or leaf node
-- as these are do not contain memoized resolution caches.
graphEdgeLookup :: GraphEdgeMemo block -> EdgeIndex -> ResolutionCache (CharacterSequence block)
graphEdgeLookup MemoGen{..} edgeIndex@(EdgeIndex src _) =
  case getTag src of
    TreeTag    ->
      case lookupEdge edgeIndex  $ treeGen (view _untaggedIndex src) of
        Just resCache -> resCache
        Nothing -> error $
          unlines
          ["graphEdgeLookup: Couldn't find edge:"
          , show edgeIndex
          , "in memoized graph edge mapping."
          ]

    NetworkTag ->
      case lookupEdge edgeIndex  $ networkGen (view _untaggedIndex src) of
        Just resCache -> resCache
        Nothing -> error $
          unlines
          ["graphEdgeLookup: Couldn't find edge:"
          , show edgeIndex
          , "in memoized graph edge mapping."
          ]
    RootTag ->
      error $
        fold
        [ "In graphEdgeLookup: We tried to find the memoized edge lookup on "
        , "a root node. This should never occur. For a root node we should "
        , "the resolutions given already by the post order"
        ]
    LeafTag ->
      error $
        fold
        [ "In graphEdgeLookup: We tried to find the memoized edge lookup on "
        , "a leaf node. This should never occur. A leaf node should never be "
        , "the source of an edge index but is in the edge: "
        , show edgeIndex
        ]

                        --------------------
                        -- Edge functions --
                        --------------------


-- |
-- Gives back a vector of all edges we wish to consider as traversal
-- edges from an unrooted network which are not adjacent to a root edge.
getUnrootedEdges
  :: Graph f c e n t
  -> Set (Either EdgeIndex a)
                -- Note: This is safe as Left is monotonic!
getUnrootedEdges = Set.mapMonotonic Left . liftA2 (<>) getNetworkEdgeSet getTreeEdgeSet


-- |
-- Given a root edge:
--
--          r                  r
--          |       or        / \
--          |                /   \
--          x               x     y
--
-- returns a vector consisting of the edges r -> x in the first case
-- and x -> y in the second. This is because when we are re-rooting
-- the binary root edge is not considered part of the associated
-- _unrooted_ tree.
getRootAdjacentEdgeSet
  :: forall f c e n t . ()
  => Graph f c e n t
  -> Set (Either EdgeIndex (Int :!: EdgeIndex))
getRootAdjacentEdgeSet graph = edgeSet
  where
    rootVec :: Vector (RootIndexData (f n) e)
    rootVec = view _rootReferences graph

    edgeSet :: Set (Either EdgeIndex (Int :!: EdgeIndex))
    edgeSet = foldMapWithKey buildEdges rootVec

    buildEdges :: Int -> RootIndexData (f n) e -> Set (Either EdgeIndex (Int :!: EdgeIndex))
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind RootTag

        childTaggedIndices :: Either TaggedIndex (TaggedIndex :!: TaggedIndex)
        childTaggedIndices = coerce $ view _childInds treeData

        oneChildHandler source target =
          Set.singleton . Left
            $ EdgeIndex {edgeSource = source, edgeTarget = target}

        twoChildHandler childInds =
          Set.singleton . Right $
            ind :!:
              EdgeIndex
                { edgeSource = view _left childInds
                , edgeTarget = view _right childInds
                }
      in
        either (oneChildHandler sourceTaggedIndex) twoChildHandler
          childTaggedIndices


-- |
-- This checks if a directed edge has a target network node i.e. the target node
-- has muiltiple parents with the original rooted graph.
isUnrootedNetworkEdge :: DirEdgeIndex -> Bool
isUnrootedNetworkEdge DirEdgeIndex{..} =
  let
    EdgeIndex{..} = edgeIndex
  in
  if reversedEdge
    then getTag edgeSource == NetworkTag
    else getTag edgeTarget == NetworkTag
