{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Intermediate where

import Data.Graph.Type
import Data.Tree (Tree(..))
import Data.Map hiding ((!))
import Data.Set
import Data.Graph.NodeContext
import Control.Lens
import Data.Vector
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Pair.Strict
import Data.Coerce

type Size = Int

data RoseForest a netRef = RoseForest
  { _forest       :: [Tree (a, Maybe netRef) ]
  , _networkNodes :: Set netRef
  }

leafR :: a -> Tree (a, Maybe netRef)
leafR name = Node (name, Nothing) []

branchR :: a -> Tree (a, Maybe netRef) -> Tree (a, Maybe netRef) -> Tree (a, Maybe netRef)
branchR name lTree rTree = Node (name, Nothing) [lTree, rTree]

networkR :: a -> netRef -> Tree (a, Maybe netRef) -> Tree (a, Maybe netRef)
networkR name netRef subTree = Node (name, Just netRef) [subTree]


data ReferenceNode l n ref = LeafRN l ref | NetworkRN !Size n ref | BranchRN !Size ref ref

data ReferenceMap l n ref = ReferenceMap
  { _rootRefs     :: [ref]
  , _networkRefs  :: [ref]
  , _refMap       :: Map ref (ReferenceNode l n ref)
  }


data BinaryTree l i n =
  LeafBT l | NetworkBT i [n] !Size  | BranchBT !Size i (BinaryTree l i n) (BinaryTree l i n)



toRoseForest
  :: forall f c e n t a netRef
  .  (t -> a)
  -> (f n -> a)
  -> (NetworkInd -> netRef)
  -> Graph f e c n t
  -> RoseForest a netRef
toRoseForest leafConv internalConv netConv graph = undefined
  where
    rootFocusGraphs = makeRootFocusGraphs graph
    build :: RootFocusGraph f e c n t -> ((a, Maybe netRef), [RootFocusGraph f e c n t])
    build (focus :!: graph) =
      case focus of
        LeafTag :!: untaggedInd ->
          let
            leafNodeInfo    = view (_leafReferences . singular (ix untaggedInd)) graph
            nodeName        = leafConv . (view _nodeData) $ leafNodeInfo
          in
            ((nodeName, Nothing), [])
        NetworkTag :!: untaggedInd ->
          let
            nodeInfo = view (_networkReferences . singular (ix untaggedInd)) graph
            nodeName = internalConv $ view _nodeData nodeInfo
            netRef   = netConv (coerce untaggedInd)
            childInd = view _childInds nodeInfo
            newFocus = toUntagged childInd
          in
            ((nodeName, Just netRef), [newFocus :!: graph])
        TreeTag    :!: untaggedInd ->
          let
            nodeInfo = view (_treeReferences . singular (ix untaggedInd)) graph
            nodeName = internalConv $ view _nodeData nodeInfo
            leftChildInd :!: rightChildInd = view _childInds nodeInfo
            leftFocus = toUntagged leftChildInd
            rightFocus = toUntagged rightChildInd
          in
            ((nodeName, Nothing), [leftFocus :!: graph, rightFocus :!: graph])
        RootTag    :!: untaggedInd ->
          let
            nodeInfo = view (_rootReferences . singular (ix untaggedInd)) graph
            nodeName = internalConv $ view _nodeData nodeInfo
            childInds = view _childInds nodeInfo
          in
            case childInds of
              Left childInd ->
                  let
                    newFocus = toUntagged childInd
                  in
                    ((nodeName, Nothing), [newFocus :!: graph])
              Right (leftChildInd :!: rightChildInd) ->
                  let
                    leftFocus  = toUntagged leftChildInd
                    rightFocus = toUntagged rightChildInd
                  in
                    ((nodeName, Nothing), [leftFocus :!: graph, rightFocus :!: graph])
          


