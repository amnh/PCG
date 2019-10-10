{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns  #-}


module Data.Graph.Internal where

import Data.Vector.Instances ()
import Control.Lens hiding (index)
import Data.Graph.Type
import Data.Graph.Memo
import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Coerce
import Data.Pair.Strict
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST
import Debug.Trace

-- |
-- This is a postorder fold function that collects together values in a monoid.
-- Note: This function double counts if the graph has multiple roots which are
-- connected via a descendent network node.
postorderFold
  :: forall f c e n t m . (Monoid m)
  => (t     -> m)
  -> ((f n) -> m)
  -> Graph f c e n t -> m
postorderFold leafFn internalFn graph =
  let
    fromNode :: TaggedIndex -> m
    fromNode (TaggedIndex untaggedInd tag) =
      case tag of
        LeafTag
          -> leafFn . (view _nodeData) . (`unsafeLeafInd` (coerce untaggedInd)) $ graph
        TreeTag    ->
          let
            treeNode :: TreeIndexData (f n) e
            treeNode   = graph `unsafeTreeInd` (coerce untaggedInd)
            childInd1, childInd2 :: ChildIndex
            childInd1  = (view (_childInds . _left )) treeNode
            childInd2  = (view (_childInds . _right)) treeNode

            childVal1, childVal2 :: m
            !childVal1 = fromNode (coerce childInd1)
            !childVal2 = fromNode (coerce childInd2)
            !childVals = childVal1 <> childVal2
          in
            childVals <> internalFn (view _nodeData treeNode)
        NetworkTag ->
          let
            networkNode :: NetworkIndexData (f n) e
            networkNode   = graph `unsafeNetworkInd` (coerce untaggedInd)
            childInd :: ChildIndex
            childInd  = (view (_childInds)) networkNode

            childVal :: m
            !childVal = fromNode (coerce childInd)
          in
            childVal
        RootTag    ->
          let
            rootNode :: RootIndexData (f n) e
            rootNode   = graph `unsafeRootInd` (coerce untaggedInd)
            childInd :: Either ChildIndex (ChildIndex :!: ChildIndex)
            childInd  = (view (_childInds)) rootNode

            childVals :: m
            !childVals =
              case childInd of
                Left singleChild
                  -> fromNode (coerce singleChild)
                Right (childInd1 :!: childInd2)
                  ->
                    let
                      !childVal1 = fromNode (coerce childInd1)
                      !childVal2 = fromNode (coerce childInd2)
                    in
                      childVal1 <> childVal2
          in
            childVals <> internalFn (view _nodeData rootNode)
  in
    undefined-- foldMap (view _rootReferences graph)

postorder
  :: forall g f e c n1 n2 t . (Applicative g)
  => (t -> n2)
  -> (g n2 -> g n2 -> g n2)
  -> Graph f c e n1 t
  -> Graph g c e n2 n2
postorder leafFn treeFn graph =
  let
    memoGen :: Endo (MemoGenGraph (g n2) n2 e)
    memoGen = memoGraphPostorder leafFn treeFn id graph

    numberL = length (graph ^. _leafReferences)
    numberI = length (graph ^. _treeReferences)
    numberN = length (graph ^. _networkReferences)
    numberR = length (graph ^. _rootReferences)

    cacheInfo :: c
    cacheInfo      = graph ^. _cachedData
  in
    generateMemoGraph cacheInfo numberL numberI numberN numberR memoGen


incrementalPostorder
  :: forall f e c n . Applicative f
  => Int
  -> (f n -> f n -> Bool)
  -> (n -> n)
  -> (n -> n -> n)
  -> Graph f c e n n
  -> Graph f c e n n
incrementalPostorder startInd thresholdFn updateFn treeFn graph = f graph
  where
    taggedIndex :: TaggedIndex
    taggedIndex = tagValue TreeTag startInd

    treeRefs        = graph ^. _treeReferences
    updatedTreeRefs = modifyNodeData startInd (fmap updateFn) treeRefs


    f :: Graph f c e n n -> Graph f c e n n
    f g = case go taggedIndex updatedTreeRefs of
      (Nothing       , v') -> g & _treeReferences .~ v'
      (Just (r, ind) , v') -> g & _treeReferences .~ v'
                                & _rootReferences  %~ (writeNodeData (getIndex ind) r)

    go
      :: TaggedIndex
      -> (Vector (TreeIndexData (f n) e))
      -> (Maybe ((f n), ParentIndex), Vector (TreeIndexData (f n) e))
    go tagInd treeVect =
      runST $
        do
          mvec <- V.thaw treeVect
          val  <- loop tagInd mvec
          vec  <- V.unsafeFreeze mvec
          pure (val, vec)


    loop :: TaggedIndex -> MVector s (TreeIndexData (f n) e) -> ST s (Maybe ((f n), ParentIndex))
    loop tagInd mvec = do
      let ind = getIndex tagInd
      currData <- MV.read mvec ind
      let currVal       = currData ^. _nodeData
      let parInd        = currData ^. _parentInds
      let parTag        = getTag $ parInd
      let
        childInds :: ChildIndex :!: ChildIndex
        childInds     = currData ^. _childInds
      let childIndData1 = graph `index` (childInds ^. _left)
      let childIndData2 = graph `index` (childInds ^. _right)
      let newVal        = (liftFunction treeFn) childIndData1 childIndData2
      let newData       = currData & _nodeData .~ newVal
      if thresholdFn currVal newVal
      then
        pure Nothing
      else
        if parTag == RootTag
        then pure (Just (newVal, parInd))
        else
          do
          MV.write mvec ind newData
          loop (coerce parInd) mvec




-- |
-- This function takes a graph of graphs, the index of one of those
-- graphs and the indices of an internal edge and returns a graph of graph
-- with the subgraph removed and added along a new node on the graph.

-- Note: this function currently assumes the parent of the leaf is a tree node
-- and the edge we are attaching to is a tree edge (an edge between two
-- tree nodes).
breakEdgeAndReattachG
  :: forall f c e n t
  .  Graph f c e n (Graph f c e n t)
  -> (ParentIndex, ChildIndex)
  -> (ParentIndex, Direction)
  -> Graph f c e n (Graph f c e n t)
breakEdgeAndReattachG graph (leafParInd, leafInd) (srcInd, dir) =
  let
    -- Parent of leafNode
    untaggedParInd = getIndex (leafParInd)
    parIndexData :: TreeIndexData (f n) e
    parIndexData =
        case getTag leafParInd of
          TreeTag -> graph `unsafeTreeInd` (TreeInd untaggedParInd)
          _       -> error "breaking an edge from a non-tree index is not yet implemented"

    parChildInfo :: ChildInfo e
    parChildInfo = view _leafParChild parIndexData

    _leafParChild, _otherLeafParChild
      :: ( HasLeft s (ChildInfo e), HasRight s (ChildInfo e))
      => Lens' s (ChildInfo e)
    _leafParChild      =
      if
        parIndexData ^. _leftChildIndex == leafInd then _left
                                         else _right
    _otherLeafParChild =
      if
        parIndexData ^. _leftChildIndex == leafInd then _right
                                         else _left


    updatedLeafParNodeInfo :: TreeIndexData (f n) e
    updatedLeafParNodeInfo  =
      parIndexData & _otherLeafParChild .~ tgtInfo
                   & _parentInds        .~ srcInd


    -- src and target contexts
    srcNodeInfo :: TreeIndexData (f n) e
    srcNodeInfo    = graph `unsafeTreeInd` (coerce (getIndex srcInd))
    tgtInfo :: ChildInfo e
    tgtInfo = srcNodeInfo ^. _srcChildLens
    tgtInd :: ChildIndex
    tgtInd       = tgtInfo ^. _childIndex
    tgtNodeInfo :: TreeIndexData (f n) e
    tgtNodeInfo  = graph `unsafeTreeInd` (coerce (getIndex tgtInd))
    untaggedSource :: Int
    untaggedSource = getIndex srcInd
    untaggedTarget :: Int
    untaggedTarget = getIndex tgtInd

    _srcChildLens :: Lens' (TreeIndexData (f n) e) (ChildInfo e)
    _srcChildLens = _getChildLens dir

    updatedSourceNodeInfo = srcNodeInfo
                          & _srcChildLens
                          .~ parChildInfo

    updatedTargetNodeInfo = tgtNodeInfo
                          & _parentInds
                          .~ leafParInd

    -- Parent and other child of above parent node
    _leafParParentChild :: Lens' (TreeIndexData (f n) e) (ChildInfo e)
    _leafParParentChild =
      if
        leafParParentNodeInfo ^. _leftChildIndex == (coerce leafParInd)
        then _right
      else _left

    leafParParentNodeInfo = graph `unsafeTreeInd` (coerce (getIndex leafParParentNodeInd))
    leafParParentNodeInd     = parIndexData ^. _parentInds
    leafParChildNodeInfo  = graph `unsafeTreeInd` (coerce (getIndex leafParParentNodeInd))
    leafParChildInfo      = parIndexData ^. _otherLeafParChild
    leafParChildNodeInd = leafParChildInfo ^. _childIndex

    updatedParParentNodeInfo = leafParParentNodeInfo
                             & _leafParParentChild
                             .~ (parIndexData ^. _otherLeafParChild)
    updatedParChildNodeInfo  = leafParChildNodeInfo
                             & _parentInds
                             .~ leafParParentNodeInd

    -- Updated Tree Reference and LeafReference
    updateTreeNodeContext :: [(Int, TreeIndexData (f n) e)]
    updateTreeNodeContext = [ (getIndex leafParParentNodeInd, updatedParParentNodeInfo)
                            , (getIndex leafParChildNodeInd , updatedParChildNodeInfo )
                            , (untaggedParInd  , updatedLeafParNodeInfo)
                            , (untaggedSource  , updatedSourceNodeInfo)
                            , (untaggedTarget  , updatedTargetNodeInfo)
                            ]

  in
    graph & _treeReferences %~ (// updateTreeNodeContext)


breadthFirstBreakAndAttach :: Int
breadthFirstBreakAndAttach = undefined
