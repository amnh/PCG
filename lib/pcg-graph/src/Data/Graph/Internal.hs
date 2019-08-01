{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

module Data.Graph.Internal where

import Data.Vector.Instances ()
import Control.Lens hiding (index)
import Data.Graph.Type
import Data.Graph.Memo
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Coerce


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

    startingValue :: f n
    startingValue = graph ^. _treeReferences . singular (ix startInd) . _nodeData


 -- TODO: rewrite using ST.
    f :: Graph f e c n n -> Graph f e c n n
    f g = case go taggedIndex (Nothing, g ^. _treeReferences) of
      (Nothing       , v') -> g & _treeReferences .~ v'
      (Just (r, ind) , v') -> g & _treeReferences .~ v'
                                & _rootReferences  %~ (modifyNodeData (untagValue ind) r)
      
 -- TODO: rewrite using ST.      
    go
      :: TaggedIndex
      -> (Maybe ((f n), ParentIndex), Vector (TreeIndexData (f n)))
      -> (Maybe ((f n), ParentIndex), Vector (TreeIndexData (f n))) 
    go tagInd ~(val, currVect) =
      let
        ind = untagValue tagInd
        currVal   = graph ^. _treeReferences . singular (ix ind) . _nodeData
        parInd    = graph ^. _treeReferences . singular (ix ind) . _parentInds
        childInds :: Pair ChildIndex
        childInds = graph ^. _treeReferences . singular (ix ind) . _childInds
        childIndData1 = graph `index` (coerce $ childInds ^. _left)
        childIndData2  = graph `index` (coerce $ childInds ^. _right)
        parTag    = getTag $ parInd
        newVal    = (liftFunction treeFn) childIndData1 childIndData2
     in
       if
         thresholdFn currVal newVal
       then
         (val, currVect)
       else
         if
           parTag == RootTag
         then (Just (newVal, parInd), currVect)
         else
           let updatedVect = undefined in
             go (coerce parInd) (val, updatedVect)
        
     
