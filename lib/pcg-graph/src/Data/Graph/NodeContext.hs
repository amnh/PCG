{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}


module Data.Graph.NodeContext where

import Data.Graph.Indices
import Control.Lens
import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Pair a = Pair {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving stock (Eq, Show)
  deriving stock Functor

class HasLeft s a | s -> a where
  _left :: Lens' s a

class HasRight s a | s -> a where
  _right :: Lens' s a

instance HasLeft (Pair a) a where
  _left = lens (\(Pair a1 _) -> a1) (\(Pair _ a2) a1' -> Pair a1' a2)

instance HasRight (Pair a) a where
  _right = lens (\(Pair _ a2) -> a2) (\(Pair a1 _) a2' -> Pair a1 a2')

data NodeIndexData d l
  = LeafNodeIndexData     (LeafIndexData l)
  | RootNodeIndexData     (RootIndexData d)
  | NetworkNodeIndexData  (NetworkIndexData d)
  | TreeNodeIndexData     (TreeIndexData d)

class HasLiftedNodeData s a where
  _liftedNodeData :: Getter s a


instance Applicative f => HasLiftedNodeData (NodeIndexData (f n) n) (f n) where
  _liftedNodeData = to get
    where
      get = \case
        LeafNodeIndexData    ind -> pure $ ind ^. _nodeData
        RootNodeIndexData    ind -> ind ^. _nodeData
        NetworkNodeIndexData ind -> ind ^. _nodeData
        TreeNodeIndexData    ind -> ind ^. _nodeData


liftFunction
  :: (Applicative f)
  => (n -> n -> n)
  -> (NodeIndexData (f n) n -> NodeIndexData (f n) n -> f n)
liftFunction fn = \nInd1 nInd2
                          -> liftA2 fn
                               (nInd1 ^. _liftedNodeData)
                               (nInd2 ^. _liftedNodeData)
                      





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

data TreeContext = TreeContext
  { parentInds :: {-# UNPACK #-} !ParentIndex
  , childInds  :: {-# UNPACK #-} !(Pair ChildIndex)
  }

type TreeIndexData d = IndexData TreeContext d


class HasParentInds s a | s -> a where
  _parentInds :: Lens' s a

class HasChildInds s a | s -> a where
  _childInds :: Lens' s a


instance HasParentInds LeafContext ParentIndex where
  _parentInds = undefined

instance HasParentInds (LeafIndexData e) ParentIndex where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds LeafContext (Pair ChildIndex) where
  _childInds = undefined

instance HasChildInds (LeafIndexData e) (Pair ChildIndex) where
  _childInds = _nodeContext . _childInds



instance HasParentInds TreeContext ParentIndex where
  _parentInds = undefined

instance HasParentInds (TreeIndexData e) ParentIndex where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds TreeContext (Pair ChildIndex) where
  _childInds = undefined

instance HasChildInds (TreeIndexData e) (Pair ChildIndex) where
  _childInds = _nodeContext . _childInds



instance HasParentInds NetworkContext (Pair ParentIndex) where
  _parentInds = undefined

instance HasParentInds (NetworkIndexData e) (Pair ParentIndex) where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds NetworkContext ChildIndex where
  _childInds = undefined

instance HasChildInds (NetworkIndexData e) ChildIndex where
  _childInds = _nodeContext . _childInds


instance HasParentInds RootContext ParentIndex where
  _parentInds = undefined

instance HasParentInds (RootIndexData e) ParentIndex where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds RootContext (Either ChildIndex (Pair ChildIndex)) where
  _childInds = undefined

instance HasChildInds (RootIndexData e) (Either ChildIndex (Pair ChildIndex)) where
  _childInds = _nodeContext . _childInds



modifyNodeData :: HasNodeData s s a a =>  Int -> a -> Vector s -> Vector s
modifyNodeData i a = V.modify
                       (\mv -> MV.modify mv (& _nodeData .~ a) i)
