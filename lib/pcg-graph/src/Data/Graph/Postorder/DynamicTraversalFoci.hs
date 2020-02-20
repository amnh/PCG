{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}

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
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import VectorBuilder.Builder (Builder)
import Data.Key (foldMapWithKey)
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (First(..))
import Data.Key (lookup)

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


lookupEdgeFull :: forall edgeData . TaggedIndex -> EdgeNetworkMapFull edgeData -> Maybe edgeData
lookupEdgeFull index EdgeNetworkMapFull{..} =
    getFirst . foldMap f $ [parentEdge, leftChildEdge, rightChildEdge]
  where
    f :: TaggedIndex :!: edgeData -> First edgeData
    f (tag :!: edgeData) | tag == index = pure edgeData
                         | otherwise    = mempty


lookupEdge :: forall edgeData . TaggedIndex -> EdgeNetworkMap edgeData -> Maybe edgeData
lookupEdge index = \case
  Left edgeMapFull -> lookupEdgeFull index edgeMapFull
  Right (EdgeNetworkMapSingle (taggedIndex :!: edgeData))
    -> if index == taggedIndex
       then pure edgeData
       else Nothing



--
--
--      (n)
--       |
--       |
--      (c)
--
newtype EdgeNetworkMapSingle edgeData =
  EdgeNetworkMapSingle
    { parentEdgeSingle     :: (TaggedIndex :!: edgeData)  -- (n) -> (c)
    }

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
  GraphShape' Identity (EdgeNetworkMap (ResolutionCache (CharacterSequence block))) ()

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
  -> Graph (ResolutionCacheM Identity) c e (CharacterSequence block) (CharacterSequence (LeafBin block))
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
        unrootedEdges :: Vector (Either EdgeIndex (Int :!: EdgeIndex))
        unrootedEdges = getRootAdjacentEdges graph <> getUnrootedEdges graph

        edgeCostMapping :: HashMap EdgeIndex (ResolutionCache (CharacterSequence block))
        edgeCostMapping =
            HashMap.fromList . Vector.toList . fmap f $ unrootedEdges
          where
            f :: Either EdgeIndex (Int :!: EdgeIndex)
              -> (EdgeIndex, ResolutionCache (CharacterSequence block))
            f = \case
              Right (rootInd :!: edge) ->
                  (edge, view _nodeData (graph `indexRoot` rootInd))

              Left nonRootEdge@(EdgeIndex src tgt) ->
                let
                  lhsContext
                    = (runIdentity <$> src `lookupTreeNode` contextNodeDatum) >>= (tgt `lookupEdge`)
                  rhsContext
                    = (runIdentity <$> tgt `lookupTreeNode` contextNodeDatum) >>= (src `lookupEdge`)
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
                      (nonRootEdge, virtualParentResolution _subBlock _subMeta meta lhs rhs)
                    Nothing         -> error errorContext

            contextNodeDatum :: GraphEdgeMapping block
            contextNodeDatum = undefined

      in
        undefined



-- |
-- Gives back a vector of all edges we wish to consider as traversal
-- edges from an unrooted network which are not adjacent to a root edge.
getUnrootedEdges
  :: Graph f c e n t
  -> Vector (Either EdgeIndex a)
getUnrootedEdges = fmap Left . (liftA2 (<>) getNetworkEdges getTreeEdges)


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
getRootAdjacentEdges
  :: forall f c e n t . ()
  => Graph f c e n t
  -> Vector (Either EdgeIndex (Int :!: EdgeIndex))
getRootAdjacentEdges graph = Builder.build rootEdgesB
  where
    rootVec :: Vector (RootIndexData (f n) e)
    rootVec = view _rootReferences graph

    rootEdgesB :: Builder (Either EdgeIndex (Int :!: EdgeIndex))
    rootEdgesB = foldMapWithKey buildEdges rootVec

    buildEdges :: Int -> RootIndexData (f n) e -> Builder (Either EdgeIndex (Int :!: EdgeIndex))
    buildEdges ind treeData =
      let
        sourceTaggedIndex :: TaggedIndex
        sourceTaggedIndex = TaggedIndex ind RootTag

        childTaggedIndices :: Either TaggedIndex (TaggedIndex :!: TaggedIndex)
        childTaggedIndices = coerce $ view _childInds treeData

        oneChildHandler source target =
          Builder.singleton $ Left $ EdgeIndex {edgeSource = source, edgeTarget = target}

        twoChildHandler childInds =
          Builder.singleton . Right $
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
  
  





  
