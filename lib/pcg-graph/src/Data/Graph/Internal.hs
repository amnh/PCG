{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}

module Data.Graph.Internal where

import Data.Vector.Instances ()
import Control.Lens hiding (index)
import Data.Graph.Type
import Data.Graph.Memo
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Coerce
import Data.Pair.Strict
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST


postorderFold
  :: (t     -> r)
  -> ((f n) -> r)
  -> ((f n) -> r -> r)
  -> ((f n) -> r -> r -> r)
  -> Graph f e c n t -> r
postorderFold = undefined


postorder
  :: forall g f e c n1 n2 t . (Applicative g)
  => (t -> n2)
  -> (n2 -> n2 -> n2)
  -> Graph f e c n1 t
  -> Graph g e c n2 n2
postorder leafFn treeFn graph =
  let
    memoGen :: Endo (MemoGenGraph (g n2) n2)
    memoGen = memoGraphPostorder leafFn treeFn id graph

    numberL = lengthOf _leafReferences     graph
    numberI = lengthOf _treeReferences graph
    numberN = lengthOf _networkReferences  graph
    numberR = lengthOf _rootReferences     graph
    cacheInfo      = graph ^. _cachedData
  in
    generateMemoGraph cacheInfo numberL numberI numberN numberR memoGen


incrementalPostorder
  :: forall f e c n . Applicative f
  => Int
  -> (f n -> f n -> Bool)
  -> (n -> n)
  -> (n -> n -> n)
  -> Graph f e c n n
  -> Graph f e c n n
incrementalPostorder startInd thresholdFn updateFn treeFn graph = f graph
  where
    taggedIndex :: TaggedIndex
    taggedIndex = tagValue TreeTag startInd

    treeRefs        = graph ^. _treeReferences
    updatedTreeRefs = modifyNodeData startInd (fmap updateFn) treeRefs


    f :: Graph f e c n n -> Graph f e c n n
    f g = case go taggedIndex updatedTreeRefs of
      (Nothing       , v') -> g & _treeReferences .~ v'
      (Just (r, ind) , v') -> g & _treeReferences .~ v'
                                & _rootReferences  %~ (writeNodeData (untagValue ind) r)
      
    go
      :: TaggedIndex
      -> (Vector (TreeIndexData (f n)))
      -> (Maybe ((f n), ParentIndex), Vector (TreeIndexData (f n)))
    go tagInd treeVect =
      runST $
        do
          mvec <- V.thaw treeVect
          val  <- loop tagInd mvec
          vec  <- V.unsafeFreeze mvec
          pure (val, vec)


    loop :: TaggedIndex -> MVector s (TreeIndexData (f n)) -> ST s (Maybe ((f n), ParentIndex))
    loop tagInd mvec = do
      let ind = untagValue tagInd
      currData <- MV.read mvec ind
      let currVal       = currData ^. _nodeData
      let parInd        = currData ^. _parentInds
      let parTag        = getTag $ parInd
      let
        childInds :: ChildIndex :!: ChildIndex
        childInds     = currData ^. _childInds
      let childIndData1 = graph `index` (coerce $ childInds ^. _left)
      let childIndData2 = graph `index` (coerce $ childInds ^. _right)
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
-- This function takes a graph and the indices of a tree node
-- and its child node and then breaks the graph into a forest
-- also returning the index of the root node of the subgraph.
breakEdge
  :: Graph f e c n t
  -> (Int, Int)
  -> (Graph f e c n t, Int)
breakEdge graph (src, tgt) = undefined


-- |
-- This function takes a graph of graphs, the index of one of those
-- graphs and returns that graph along with the graph of graphs with
-- that node removed.
breakEdgeGraph
  :: Graph f e c n (Graph f e c n t)
  -> Int
  -> (Graph f e c n t, Graph f e c n (Graph f e c n t))
breakEdgeGraph graph subgraphInd =
  let
    leaves = graph ^. _leafReferences
    (before, after) = V.splitAt subgraphInd leaves
    updatedAfter    = fmap (undefined ) . V.tail $ after
    newLeaves       = before <> updatedAfter
  in
    undefined
  
  
--}
