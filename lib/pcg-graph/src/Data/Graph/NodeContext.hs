{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.Graph.NodeContext where

import Data.Graph.Indices
import Control.Lens

data Pair a = Pair {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving stock (Eq, Show)
  deriving stock Functor

class HasLeft s a | s -> a where
  _left :: Lens' s a

class HasRight s a | s -> a where
  _right :: Lens' s a

instance HasLeft (Pair a) a where
  _left = lens (\(Pair a1 a2) -> a1) (\(Pair a1 a2) a1' -> Pair a1' a2)

instance HasRight (Pair a) a where
  _right = lens (\(Pair a1 a2) -> a2) (\(Pair a1 a2) a2' -> Pair a1 a2')

data NodeContext
  = LeafC     LeafContext
  | RootC     RootContext
  | NetworkC  NetworkContext
  | InternalC InternalContext

leafNodeContext :: LeafContext -> NodeContext
leafNodeContext = LeafC
rootNodeContext :: RootContext -> NodeContext
rootNodeContext = RootC
networkNodeContext :: NetworkContext -> NodeContext
networkNodeContext = NetworkC
internalNodeContext :: InternalContext -> NodeContext
internalNodeContext = InternalC


data IndexData nodeContext nodeData  = IndexData
  { nodeData    :: nodeData
  , nodeContext :: !nodeContext
  }
  deriving stock (Eq, Functor)

class HasNodeData s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _nodeData :: Lens s t a b

class HasNodeContext s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _nodeContext :: Lens s t a b

instance HasNodeData (IndexData nc nd) (IndexData nc nd') nd nd' where
  _nodeData = lens nodeData (\ind nd -> ind {nodeData = nd})


instance HasNodeContext (IndexData nc nd) (IndexData nc' nd) nc nc'  where
  _nodeContext = lens nodeContext (\ind nc -> ind {nodeContext = nc})


newtype RootContext = RootContext
  { childInds :: (Either ChildIndex (Pair ChildIndex))
  }
type RootIndexData d = IndexData RootContext d

newtype LeafContext = LeafContext
  { parentInds :: ParentIndex
  }
type LeafIndexData d = IndexData LeafContext d

data NetworkContext = NetworkContext
  { parentInds  :: {-# UNPACK #-} !(Pair ParentIndex)
  , childInds   :: {-# UNPACK #-} !ChildIndex
  }

type NetworkIndexData d = IndexData NetworkContext d

data InternalContext = InternalContext
  { parentInds :: {-# UNPACK #-} !ParentIndex
  , childInds  :: {-# UNPACK #-} !(Pair ChildIndex)
  }

type InternalIndexData d = IndexData InternalContext d

class HasParentInds s a | s -> a where
  _parentInds :: Lens' s a

class HasChildInds s a | s -> a where
  _childInds :: Lens' s a

instance HasParentInds LeafContext ParentIndex where
  _parentInds = undefined

instance HasParentInds InternalContext ParentIndex where
  _parentInds = undefined

instance HasParentInds NetworkContext (Pair ParentIndex) where
  _parentInds = undefined

instance HasChildInds RootContext (Either ChildIndex (Pair ChildIndex)) where
  _childInds = undefined

instance HasChildInds InternalContext (Pair ChildIndex) where
  _childInds = undefined

instance HasChildInds NetworkContext ChildIndex where
  _childInds = undefined

instance HasChildInds ctxt ChildIndex => HasChildInds (IndexData ctxt n) ChildIndex where
  _childInds = _nodeContext . _childInds
