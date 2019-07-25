{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Internal where

import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Vector (Vector)
import Data.Key
import Data.Vector.Instances ()
import Control.Lens
import Data.Bifunctor
import Data.Graph.Type
import Data.Graph.Memo
import Control.Applicative


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
