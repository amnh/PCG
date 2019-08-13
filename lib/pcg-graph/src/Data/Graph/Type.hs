{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Type where

import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Vector (Vector)
import Data.Kind (Type)
import Data.Vector.Instances ()
import Control.Lens

data GraphShape i n r t
  = GraphShape
  { leafData     :: Vector t
  , treeData :: Vector i
  , networkData  :: Vector n
  , rootData     :: Vector r
  }

data Graph
       (f :: Type -> Type)
       (c :: Type)
       (e :: Type)
       (n :: Type)
       (t :: Type)
  = Graph
  { leafReferences     :: Vector (LeafIndexData     (  t))
  , treeReferences     :: Vector (TreeIndexData     (f n))
  , networkReferences  :: Vector (NetworkIndexData  (f n))
  , rootReferences     :: Vector (RootIndexData     (f n))
  , cachedData         :: c
  }

instance Functor f => Bifunctor (Graph f c e) where
  bimap f g graph@(Graph{..}) =
    graph
      { leafReferences     = fmap (fmap g) leafReferences
      , treeReferences     = fmap (fmap (fmap f)) treeReferences
      , networkReferences  = fmap (fmap (fmap f)) networkReferences
      , rootReferences     = fmap (fmap (fmap f)) rootReferences
      }

class HasLeafReferences s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _leafReferences :: Lens s t a b

instance HasLeafReferences
           (Graph f c e n t)
           (Graph f c e n t')
           (Vector (IndexData LeafContext t))
           (Vector (IndexData LeafContext t')) where
  _leafReferences = lens leafReferences (\g l -> g { leafReferences = l})


class HasTreeReferences s a | s -> a where
  _treeReferences :: Lens' s a

instance HasTreeReferences
           (Graph f c e n t)
           (Vector (IndexData TreeContext (f n))) where
  _treeReferences = lens treeReferences (\g fn -> g {treeReferences = fn})

class HasNetworkReferences s a | s -> a where
  _networkReferences :: Lens' s a

instance HasNetworkReferences
           (Graph f c e n t)
           (Vector (IndexData NetworkContext (f n))) where
  _networkReferences = lens networkReferences (\g fn -> g {networkReferences = fn})

class HasRootReferences s a | s -> a where
  _rootReferences :: Lens' s a

instance HasRootReferences
           (Graph f c e n t)
           (Vector (IndexData RootContext (f n))) where
  _rootReferences = lens rootReferences (\g fn -> g {rootReferences = fn})

class HasCachedData s t a b | s -> a, t -> b, s b -> t, t a -> b where
  _cachedData :: Lens s t a b

instance HasCachedData
           (Graph f c1 e n t)
           (Graph f c2 e n t)
           c1
           c2 where
  _cachedData = lens cachedData (\g c2 -> g {cachedData = c2})



index :: Tagged taggedInd => Graph f c e n t -> taggedInd -> NodeIndexData (f n) t
index graph taggedIndex =
  let
    ind = untagValue taggedIndex
  in
  case getTag taggedIndex of
    LeafTag    -> LeafNodeIndexData $
                     graph ^.
                      _leafReferences
                    . (singular $ ix ind)

    TreeTag -> TreeNodeIndexData $
                     graph ^.
                       _treeReferences
                     . (singular $ ix ind)

    NetworkTag  -> NetworkNodeIndexData $
                    graph ^.
                       _networkReferences
                     . (singular $ ix ind)

    RootTag     -> RootNodeIndexData $
                    graph ^.
                       _rootReferences
                     . (singular $ ix ind)



--      ┌───────────────────────┐
--      │    Unsafe Indexing    │
--      └───────────────────────┘

{-# INLINE unsafeLeafInd #-}
unsafeLeafInd    :: Graph f c e n t -> LeafInd -> LeafIndexData t
unsafeLeafInd graph (LeafInd i) = graph ^. _leafReferences . (singular (ix i))

{-# INLINE unsafeTreeInd #-}
unsafeTreeInd    :: Graph f c e n t -> TreeInd -> TreeIndexData (f n)
unsafeTreeInd graph (TreeInd i) = graph ^. _treeReferences . (singular (ix i))

{-# INLINE unsafeRootInd #-}
unsafeRootInd    :: Graph f c e n t -> RootInd -> RootIndexData (f n)
unsafeRootInd graph (RootInd i) = graph ^. _rootReferences . (singular (ix i))

{-# INLINE unsafeNetworkInd #-}
unsafeNetworkInd :: Graph f c e n t -> NetworkInd -> NetworkIndexData (f n)
unsafeNetworkInd graph (NetworkInd i) = graph ^. _networkReferences . (singular (ix i))
