{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}


module Data.Graph.NodeContext where

import Data.Graph.Indices
import Control.Lens
import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Pair.Strict


--      ┌──────────────────────────────┐
--      │    Polymorphic Index Data    │
--      └──────────────────────────────┘

data LabelledChildIndex e =
  LabelledChildIndex
  { edgeAnnotation :: e
  , labelledChildIndex :: {-# UNPACK #-} !ChildIndex
  }

data IndexData nodeContext nodeData  = IndexData
  { nodeData    :: nodeData
  , nodeContext :: !nodeContext
  }
  deriving stock (Eq, Functor, Show)

instance Bifunctor IndexData where
  bimap f g (IndexData n d) =
    IndexData
    { nodeData = g n
    , nodeContext = f d
    }

class HasNodeData s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _nodeData :: Lens s t a b

class HasNodeContext s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _nodeContext :: Lens s t a b

instance HasNodeData (IndexData nc nd) (IndexData nc nd') nd nd' where
  _nodeData = lens nodeData (\ind nd -> ind {nodeData = nd})


instance HasNodeContext (IndexData nc nd) (IndexData nc' nd) nc nc'  where
  _nodeContext = lens nodeContext (\ind nc -> ind {nodeContext = nc})


--      ┌──────────────────────────────┐
--      │    Node Contexts and Data    │
--      └──────────────────────────────┘

newtype RootContext e = RootContext
  { childInfoR :: (Either (ChildInfo e) ((ChildInfo e) :!: (ChildInfo e)))
  }
  deriving stock Show

type RootIndexData d e = IndexData (RootContext e) d

rootIndexData
  :: d
  -> Either (ChildInfo e) ((ChildInfo e) :!: (ChildInfo e))
  -> RootIndexData d e
rootIndexData d inds =
  IndexData
  { nodeData = d
  , nodeContext = RootContext inds
  }

newtype LeafContext = LeafContext
  { parentIndsL :: ParentIndex
  }
  deriving stock (Show)

type LeafIndexData d = IndexData LeafContext d

leafIndexData :: d -> ParentIndex -> LeafIndexData d
leafIndexData d ind =
  IndexData
  { nodeData = d
  , nodeContext = LeafContext ind
  }

data NetworkContext e = NetworkContext
  { parentIndsN  :: {-# UNPACK #-} !(ParentIndex :!: ParentIndex)
  , childInfoN   :: {-# UNPACK #-} !(ChildInfo e)
  }
  deriving stock Show

type NetworkIndexData d e = IndexData (NetworkContext e) d

networkIndexData
  :: d
  -> ParentIndex :!: ParentIndex
  -> (ChildInfo e)
  -> NetworkIndexData d e
networkIndexData d parInd childInd =
  IndexData
  { nodeData = d
  , nodeContext
      = NetworkContext
          { parentIndsN = parInd
          , childInfoN  = childInd
          }
  }

data TreeContext e = TreeContext
  { parentIndsT :: {-# UNPACK #-} !ParentIndex
  , childInfoT  :: {-# UNPACK #-} !((ChildInfo e) :!: (ChildInfo e))
  }
  deriving stock (Show)

type TreeIndexData d e = IndexData (TreeContext e) d

treeIndexData
  :: d
  -> ParentIndex
  -> ChildInfo e :!: ChildInfo e
  -> TreeIndexData d e
treeIndexData d parInd childInd =
  IndexData
  { nodeData = d
  , nodeContext
      = TreeContext
          { parentIndsT = parInd
          , childInfoT  = childInd
          }
  }


-- |
-- This is for use to say which child we are looking at
data Direction = L | R

_getChildLens :: (HasLeft s a, HasRight s a) => Direction -> Lens' s a
_getChildLens =
  \case
    L -> _left
    R -> _right

_getOtherChildLens
  :: (HasLeft s a, HasRight s a) => Direction -> Lens' s a
_getOtherChildLens =
  \case
    L -> _right
    R -> _left


--      ┌──────────────────────────────────┐
--      │    Type classes for reindexing   │
--      └──────────────────────────────────┘

class Reindexable s where
  increment :: s -> s
  decrement :: s -> s



--      ┌─────────────────────┐
--      │    Classy Lenses    │
--      └─────────────────────┘


class HasParentInds s a | s -> a where
  _parentInds :: Lens' s a

class HasChildInfo s a | s -> a where
  _childInfo :: Lens' s a

class HasChildInds s a | s -> a where
   _childInds :: Getter s a

class HasLeftChildIndex s a | s -> a where
  _leftChildIndex :: Getter s a

class HasRightChildIndex s a | s -> a where
  _rightChildIndex :: Getter s a

class HasLeftChildInfo s a | s -> a where
  _leftChildInfo :: Lens' s a

class HasRightChildInfo s a | s -> a where
  _rightChildInfo :: Lens' s a



--      ┌─────────────────────┐
--      │    Leaf Accessors   │
--      └─────────────────────┘
instance HasParentInds LeafContext ParentIndex where
  _parentInds = lens parentIndsL  (\l p -> l {parentIndsL = p})

instance HasParentInds (LeafIndexData e) ParentIndex where
  _parentInds = _nodeContext . _parentInds



--      ┌─────────────────────┐
--      │    Tree Accessors   │
--      └─────────────────────┘
instance HasParentInds (TreeContext e) ParentIndex where
  _parentInds =  lens parentIndsT (\l c -> l {parentIndsT = c})

instance HasParentInds (TreeIndexData d e) ParentIndex where
  _parentInds = _nodeContext . _parentInds

instance HasChildInfo (TreeContext e) ((ChildInfo e) :!: (ChildInfo e)) where
  _childInfo = lens childInfoT (\l c -> l {childInfoT = c})

instance HasChildInds (TreeContext e) (ChildIndex :!: ChildIndex) where
  _childInds = withinP _childInfo _childIndex


instance HasChildInfo (TreeIndexData d e) (ChildInfo e :!: ChildInfo e) where
  _childInfo = _nodeContext . _childInfo

instance HasChildInds (TreeIndexData d e) (ChildIndex :!: ChildIndex) where
  _childInds = withinP _childInfo _childIndex


instance HasLeft (TreeIndexData d e) (ChildInfo e) where
  _left = _childInfo . _left

instance HasRight (TreeIndexData d e) (ChildInfo e) where
  _right = _childInfo . _right

instance HasLeftChildIndex (TreeIndexData d e) ChildIndex where
  _leftChildIndex = _childInds . _left

instance HasRightChildIndex (TreeIndexData d e) ChildIndex where
  _rightChildIndex = _childInds . _right



--      ┌────────────────────────┐
--      │    Network Accessors   │
--      └────────────────────────┘
instance HasParentInds (NetworkContext e) (Pair ParentIndex ParentIndex) where
  _parentInds = lens parentIndsN (\l c -> l {parentIndsN = c})

instance HasParentInds (NetworkIndexData d e) (Pair ParentIndex ParentIndex) where
  _parentInds = _nodeContext . _parentInds

instance HasChildInfo (NetworkContext e) (ChildInfo e) where
  _childInfo = lens childInfoN (\l c -> l {childInfoN = c})

instance HasChildInds (NetworkContext e) (ChildIndex) where
  _childInds = _childInfo . _childIndex

instance HasChildInfo (NetworkIndexData d e) (ChildInfo e) where
  _childInfo = _nodeContext . _childInfo

instance HasChildInds (NetworkIndexData d e) (ChildIndex) where
  _childInds = _childInfo . _childIndex


--      ┌─────────────────────┐
--      │    Root Accessors   │
--      └─────────────────────┘

instance HasChildInfo (RootContext e)
                      (Either (ChildInfo e) ((ChildInfo e) :!: (ChildInfo e))) where
  _childInfo = lens childInfoR (\l c -> l {childInfoR = c})

instance HasChildInds (RootContext e) (Either ChildIndex (ChildIndex :!: ChildIndex)) where
  _childInds =
    let
      getChildInds
        :: Either (ChildInfo e) ((ChildInfo e) :!: (ChildInfo e))
        -> Either ChildIndex    (ChildIndex :!: ChildIndex)
      getChildInds = bimap (view _childIndex) (view (_both _childIndex))
      _view :: RootContext e -> Either ChildIndex (ChildIndex :!: ChildIndex)
      _view = getChildInds . (view _childInfo)
    in
      to _view


instance HasChildInfo (RootIndexData d e)
                      (Either (ChildInfo e) ((ChildInfo e) :!: (ChildInfo e))) where
  _childInfo = _nodeContext . _childInfo

instance HasChildInds (RootIndexData d e)
                      (Either ChildIndex (ChildIndex :!: ChildIndex)) where
  _childInds = _nodeContext . _childInds

writeNodeData :: HasNodeData s s a a =>  Int -> a -> Vector s -> Vector s
writeNodeData i a = V.modify
                       (\mv -> MV.modify mv (& _nodeData .~ a) i)


modifyNodeData :: HasNodeData s s a a =>  Int -> (a -> a) -> Vector s -> Vector s
modifyNodeData i fn = V.modify
                       (\mv -> MV.modify mv (& _nodeData %~ fn) i)




--      ┌──────────────────────────┐
--      │    Combined Index Data   │
--      └──────────────────────────┘

data NodeIndexData d e l
  = LeafNodeIndexData     (LeafIndexData l)
  | RootNodeIndexData     (RootIndexData d e)
  | NetworkNodeIndexData  (NetworkIndexData d e)
  | TreeNodeIndexData     (TreeIndexData d e)

class HasLiftedNodeData s a where
  _liftedNodeData :: Getter s a


instance Applicative f => HasLiftedNodeData (NodeIndexData (f n) e n) (f n) where
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
  -> (NodeIndexData (f n) e n -> NodeIndexData (f n) e n -> f n)
liftFunction fn = \nInd1 nInd2
                          -> liftA2 fn
                               (nInd1 ^. _liftedNodeData)
                               (nInd2 ^. _liftedNodeData)
