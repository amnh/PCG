{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}

module Data.Graph.Internal where

import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Vector (Vector)
-- import qualified Data.Vector as V
import Data.Kind (Type)
import Data.Key
import Data.Vector.Instances ()


data Graph
       (f :: Type -> Type)
       (c :: Type)
       (e :: Type)
       (n :: Type)
       (t :: Type)
  = Graph
  { leafReferences     :: Vector (IndexData (  t))
  , internalReferences :: Vector (IndexData (f n))
  , networkReferences  :: Vector (IndexData (f n))
  , rootReferences     :: Vector (IndexData (f n))
  , cachedData         :: c
  }


index :: Graph f c e n t -> TaggedIndex -> NodeContext
index graph taggedIndex =
  let
    ind = untagValue taggedIndex
  in
  case getTag taggedIndex of
    Leaf     -> nodeContext . (! ind) .  leafReferences     $ graph 
    Internal -> nodeContext . (! ind) .  internalReferences $ graph
    Network  -> nodeContext . (! ind) .  networkReferences  $ graph
    Root     -> nodeContext . (! ind) .  rootReferences     $ graph

  





