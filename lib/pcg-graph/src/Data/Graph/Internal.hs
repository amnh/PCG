{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

module Data.Graph.Internal where

import Data.Vector.Instances ()
import Control.Lens
import Data.Graph.Type
import Data.Graph.Memo


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
postorder leafFn internalFn graph =
  let
    memoGen :: Endo (MemoGenGraph (g n2) n2)
    memoGen = memoGraphPostorder leafFn internalFn id graph

    numberL = lengthOf _leafReferences     graph
    numberI = lengthOf _internalReferences graph
    numberN = lengthOf _networkReferences  graph
    numberR = lengthOf _rootReferences     graph
    cacheInfo      = graph ^. _cachedData
  in
    generateMemoGraph cacheInfo numberL numberI numberN numberR memoGen


incrementalPostorder
  :: forall g f e c n t .
  -> Int
  -> (n -> n -> True)
  -> (n -> n)
  -> (n -> n -> n)
  -> Graph f e c n n
  -> Graph f e c n n
incrementalPostorder startInd thresholdFn updateFn internalFn graph =
    graph %~ _internalReferences
  where
    taggedIndex = tagValue InternalTag internalInd

    startingValue :: n
    startingValue = graph ^. _internalReferences . singular (ix i)

 -- This would be more efficient using the ST monad
    go :: TaggedIndex -> (Maybe n, Vector (InternalIndexData (f n)))
    go tagIndex currValue ~(val, currVect) =
      let
        currVal   = graph ^. _internalReferences . singular (ix i) . _nodeData
        parInd    = graph ^. _internalReferences . singular (ix i) . _parentInds
        childInds = graph ^. _internalReferences . singular (ix i) . _childInds
        parTag    = getTag $ parInd
        newVal    = internalFn
                      (childInds ^. _left)
                      (childInds ^. _right)
     in
       if parTag == RootTag
         then (Just newVal, currVect)
         else
           let updatedVect = undefined in
             go parInd (val, updatedVect)
        
     
