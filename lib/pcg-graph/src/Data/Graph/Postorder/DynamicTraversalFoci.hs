{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}

module Data.Graph.Postorder.DynamicTraversalFoci where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph.Postorder.Resolution
import Data.Graph.Type
import Data.Graph.Sequence.Class
import Data.Pair.Strict
import Control.Lens hiding (index)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Applicative
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Key
import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (First(..))
import Data.Key (lookup)
import Data.Graph.Memo
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)

--      (i)
--       |
--      (n)
--     /   \
--   (j)   (k)
data EdgeTreeMap edge edgeData = EdgeTreeMap
  { parentEdgeT     :: !(edge :!: edgeData)  -- (n) -> (i)
  , leftChildEdgeT  :: !(edge :!: edgeData)  -- (n) -> (j)
  , rightChildEdgeT :: !(edge :!: edgeData)  -- (n) -> (k)
  }

--      (i)
--       |
--      (n)
--     /   \
--   (j)   (k)
data EdgeNetworkMapFull edgeData =
    EdgeNetworkMapFull
    { parentEdge     :: !(TaggedIndex :!: edgeData)  -- (n) -> (i)
    , leftChildEdge  :: !(TaggedIndex :!: edgeData)  -- (n) -> (j)
    , rightChildEdge :: !(TaggedIndex :!: edgeData)  -- (n) -> (k)
    }

type NodeArrangement = EdgeNetworkMapFull ()

pattern NodeArrangement :: TaggedIndex -> TaggedIndex -> TaggedIndex -> EdgeNetworkMapFull ()
pattern NodeArrangement p l r <- EdgeNetworkMapFull (p :!: _) (l :!: _) (r :!: _)
  where
    NodeArrangement p l r = EdgeNetworkMapFull (p :!: ()) (l :!: ()) (r :!: ())


_parentEdge :: Lens' NodeArrangement TaggedIndex
_parentEdge = lens (view _left . parentEdge) (\g l -> g { parentEdge = l :!: ()})

_leftChildEdge :: Lens' NodeArrangement TaggedIndex
_leftChildEdge = lens (view _left . leftChildEdge) (\g l -> g { leftChildEdge = l :!: ()})

_rightChildEdge :: Lens' NodeArrangement TaggedIndex
_rightChildEdge = lens (view _left . rightChildEdge) (\g l -> g { rightChildEdge = l :!: ()})


lookupEdge :: forall edgeData . TaggedIndex -> EdgeNetworkMapFull edgeData -> Maybe edgeData
lookupEdge index EdgeNetworkMapFull{..} =
    getFirst . foldMap f $ [parentEdge, leftChildEdge, rightChildEdge]
  where
    f :: TaggedIndex :!: edgeData -> First edgeData
    f (tag :!: edgeData) | tag == index = pure edgeData
                         | otherwise    = mempty


--lookupEdge :: forall edgeData . TaggedIndex -> EdgeNetworkMap edgeData -> Maybe edgeData
--lookupEdge index = \case
--  Left edgeMapFull -> lookupEdgeFull index edgeMapFull
--  Right (EdgeNetworkMapSingle (taggedIndex :!: edgeData))
--    -> if index == taggedIndex
--       then pure edgeData
--       else Nothing



--
--
--      (n)
--       |
--       |
--      (c)
--
data EdgeNetworkMapSingle edgeData =
    EdgeNetworkMapSingle (TaggedIndex :!: edgeData)  -- (n) -> (c)
  | NoEdgeMapData
  deriving Show
    

type EdgeNetworkMap edgeData = Either (EdgeNetworkMapFull edgeData) (EdgeNetworkMapSingle edgeData)



type Topology = ()
  -- to do: incorporate topology

type NonExactCharacterMapping = HashMap Topology NonExactCharacterTraversals

type NonExactCharacterTraversals = NonEmpty NonExactCharacterInfo

data NonExactCharacterInfo = NonExactCharacterInfo
  { cost ::  Double
  , minimalCostEdges :: Vector (Word :!: NonEmpty EdgeIndex)
  }


type GraphEdgeMapping block =
  GraphShape
    (EdgeNetworkMapFull (ResolutionCache (CharacterSequence block)))
    (EdgeNetworkMapFull (ResolutionCache (CharacterSequence block)))
    (EdgeNetworkMap     (ResolutionCache (CharacterSequence block)))
    ()

emptyEdgeMapping :: GraphEdgeMapping block
emptyEdgeMapping =
  GraphShape mempty mempty mempty mempty

assignOptimalDynamicCharacterRootEdges
  :: forall f block subBlock meta c e .
     ( BlockBin block
     , BlockBin subBlock
     , HasSequenceCost subBlock
     , HasSequenceCost block
     )
  => Lens' block subBlock
  -> Lens' (LeafBin block) (LeafBin subBlock)
  -> Lens' (CharacterMetadata block) (CharacterMetadata subBlock)
  -> MetadataSequence block meta
  -> Graph
       (ResolutionCacheM Identity) c e
       (CharacterSequence block) (CharacterSequence (LeafBin block))
  -> ( HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
     , GraphEdgeMapping block
     )
assignOptimalDynamicCharacterRootEdges _subBlock _subLeaf _subMeta meta graph =
  case numberOfNodes graph of
    0 -> (mempty, emptyEdgeMapping)
    1 -> (mempty, emptyEdgeMapping)
    2 ->
      let
        rootCache :: ResolutionCache (CharacterSequence block)
        rootCache  = (view _nodeData) . Vector.head $ (view _rootReferences graph)
        childInd = undefined
        edge :: EdgeIndex
        edge = undefined
        edgeMap :: EdgeNetworkMap (ResolutionCache (CharacterSequence block))
        edgeMap = Right $ EdgeNetworkMapSingle (childInd :!: rootCache)
        mapping = HashMap.fromList [(edge, rootCache)]
        graphMapping :: GraphEdgeMapping block
        graphMapping
          = GraphShape mempty mempty mempty (coerce . Vector.singleton $ edgeMap)
      in
        (mapping, graphMapping)


    _ ->
      let
        unrootedEdges :: Set (Either EdgeIndex (Int :!: EdgeIndex))
        unrootedEdges = getRootAdjacentEdgeSet graph <> getUnrootedEdges graph

        edgeCostMapping :: HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
        edgeCostMapping = HashMap.fromList . fmap f . Set.toList $ unrootedEdges
          where
          f :: Either EdgeIndex (Int :!: EdgeIndex)
            -> (EdgeIndex, ResolutionCache (CharacterSequence block))
          f = \case
            Right (rootInd :!: edge) ->
                (edge, view _nodeData (graph `indexRoot` rootInd))
            Left nonRootEdge@(EdgeIndex src tgt) ->
              let
                lhsContext
                  = (src `lookupTreeNode` contextNodeDatum) >>= (tgt `lookupEdge`)
                rhsContext
                  = (tgt `lookupTreeNode` contextNodeDatum) >>= (src `lookupEdge`)
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

          numRootNodes, numNetNodes, numLeafNodes, numTreeNodes :: Int
          numRootNodes = length (view _rootReferences graph)
          numNetNodes  = length (view _networkReferences graph)
          numLeafNodes = length (view _leafReferences graph)
          numTreeNodes = length (view _treeReferences graph)

          contextNodeDatum :: GraphEdgeMapping block
          contextNodeDatum =
            generateMemoGraphShape
              numLeafNodes
              numTreeNodes
              numNetNodes
              numLeafNodes
              recursiveContextDatum

          recursiveContextDatum
            :: Endo
                (MemoGen
                  (EdgeNetworkMapFull (ResolutionCache (CharacterSequence block)))
                  (EdgeNetworkMapFull (ResolutionCache (CharacterSequence block)))
                  (EdgeNetworkMap     (ResolutionCache (CharacterSequence block)))
                  ()
                )
          recursiveContextDatum
            MemoGen
            { leafGen    = recLeafFn
            , treeGen    = recTreeFn
            , networkGen = recNetworkFn
            , rootGen    = recRootFn
            } = MemoGen{..}
            where
            rootGen = const (Right NoEdgeMapData)
            leafGen = const ()
            networkGen = undefined
            treeGen = undefined
            
            directedEdgeDatum = undefined
      in
        undefined

deriveDirectedEdgeDatum
  :: forall block meta c e . (HasMetadataSequence c (MetadataSequence block meta))
  => Graph
       (ResolutionCacheM Identity)
       c e
       (CharacterSequence block)
       (CharacterSequence (LeafBin block))
  -> TaggedIndex
  -> [TaggedIndex]
  -> NodeArrangement
  -> [(EdgeIndex, ResolutionCache (CharacterSequence block))]
deriveDirectedEdgeDatum graph nodeInd parNodes (NodeArrangement p l r) =
  [(EdgeIndex p nodeInd, subTreeResolutions)]
  where
    meta = view (_cachedData . _metadataSequence) graph
    subTreeResolutions | p `elem` parNodes = getResolutionCache meta nodeInd graph
    


getUnrootedEdgeParent :: TaggedIndex -> Graph f c e n t -> [TaggedIndex]
getUnrootedEdgeParent nodeIndex graph
  | not . hasRootParent nodeIndex $ graph = toList $ getSibling nodeIndex graph
  | otherwise                             = getParents nodeIndex graph


getNodeContext :: TaggedIndex -> Graph f c e n t -> Maybe NodeArrangement
getNodeContext = undefined
     
  
allNodeArrangements :: NodeArrangement -> NonEmpty NodeArrangement
allNodeArrangements arrange@(NodeArrangement p l r)
  = arrange :| [(NodeArrangement l r p), (NodeArrangement r p l)]
  


filterEdges
  :: Set EdgeIndex
  -> ResolutionCache (CharacterSequence block)
  -> [Resolution (CharacterSequence block)]
filterEdges edges = runIdentity . filterResolution hasEdge
  where
    hasEdge :: Resolution (CharacterSequence block) -> Bool
    hasEdge res =
      let
        resolutionEdges = view _subTreeEdgeSet res
      in
        edges `Set.disjoint` resolutionEdges


-- |
-- Gives back a vector of all edges we wish to consider as traversal
-- edges from an unrooted network which are not adjacent to a root edge.
getUnrootedEdges
  :: Graph f c e n t
  -> Set (Either EdgeIndex a)
                -- Note: This is safe as Left is monotonic!
getUnrootedEdges = Set.mapMonotonic Left . (liftA2 (<>) getNetworkEdgeSet getTreeEdgeSet)


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
          Set.singleton $ Left $ EdgeIndex {edgeSource = source, edgeTarget = target}

        twoChildHandler childInds =
          Set.singleton . Right $
            ind :!:
              EdgeIndex {edgeSource = view _left childInds, edgeTarget = view _right childInds}
      in
        either (oneChildHandler sourceTaggedIndex) twoChildHandler
          $ childTaggedIndices


-- |
-- This checks if an edge has a target network node i.e. the target node
-- has muiltiple parents.
isNetworkEdge :: EdgeIndex -> Bool
isNetworkEdge EdgeIndex{..} = getTag edgeTarget == NetworkTag


unsafeLookup :: (Lookup f, Show (Key f)) => f a -> Key f -> a
unsafeLookup s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s
  





  
