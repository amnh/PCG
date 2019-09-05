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

data IndexData nodeContext nodeData  = IndexData
  { nodeData    :: nodeData
  , nodeContext :: !nodeContext
  }
  deriving stock (Eq, Functor)

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

newtype RootContext = RootContext
  { childIndsR :: (Either ChildIndex (ChildIndex :!: ChildIndex))
  }
type RootIndexData d = IndexData RootContext d

rootIndexData :: d -> Either ChildIndex (ChildIndex :!: ChildIndex) -> RootIndexData d
rootIndexData d inds =
  IndexData
  { nodeData = d
  , nodeContext = RootContext inds
  }

newtype LeafContext = LeafContext
  { parentIndsL :: ParentIndex
  }
type LeafIndexData d = IndexData LeafContext d

leafIndexData :: d -> ParentIndex -> LeafIndexData d
leafIndexData d ind =
  IndexData
  { nodeData = d
  , nodeContext = LeafContext ind
  }

data NetworkContext = NetworkContext
  { parentIndsN  :: {-# UNPACK #-} !(ParentIndex :!: ParentIndex)
  , childIndsN   :: {-# UNPACK #-} !ChildIndex
  }

type NetworkIndexData d = IndexData NetworkContext d

networkIndexData :: d -> ParentIndex :!: ParentIndex -> ChildIndex -> NetworkIndexData d
networkIndexData d parInd childInd =
  IndexData
  { nodeData = d
  , nodeContext
      = NetworkContext
          { parentIndsN = parInd
          , childIndsN  = childInd
          }
  }

data TreeContext = TreeContext
  { parentIndsT :: {-# UNPACK #-} !ParentIndex
  , childIndsT  :: {-# UNPACK #-} !(ChildIndex :!: ChildIndex)
  }

type TreeIndexData d = IndexData TreeContext d

treeIndexData :: d -> ParentIndex -> ChildIndex :!: ChildIndex -> TreeIndexData d
treeIndexData d parInd childInd =
  IndexData
  { nodeData = d
  , nodeContext
      = TreeContext
          { parentIndsT = parInd
          , childIndsT  = childInd
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

_getOtherChildLens :: (HasLeft s a, HasRight s a) => Direction -> Lens' s a
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


instance Reindexable RootContext where
  increment rc =
    rc & _childInds %~
          (\case
             Left  c -> Left $ c + 1
             Right p -> Right $ bimap (+ 1) (+ 1) p)

  decrement rc =
    rc & _childInds %~
          (\case
             Left  c -> Left $ c - 1
             Right p -> Right $ bimap (subtract 1) (subtract 1) p)

instance Reindexable LeafContext where
  increment rc =
    rc & _parentInds %~ (+ 1)

  decrement rc =
    rc & _parentInds %~ (subtract 1)


instance Reindexable NetworkContext where
  increment rc =
    rc & _parentInds %~ (bimap (+ 1) (+ 1))
       & _childInds  %~ (+ 1)

  decrement rc =
    rc & _parentInds %~ (bimap (subtract 1) (subtract 1))
       & _childInds  %~ (subtract 1)


instance Reindexable TreeContext where
  increment rc =
    rc & _childInds   %~ (bimap (+ 1) (+ 1))
       & _parentInds  %~ (+ 1)

  decrement rc =
    rc & _childInds   %~ (bimap (subtract 1) (subtract 1))
       & _parentInds  %~ (subtract 1)



--      ┌─────────────────────┐
--      │    Classy Lenses    │
--      └─────────────────────┘


class HasParentInds s a | s -> a where
  _parentInds :: Lens' s a

class HasChildInds s a | s -> a where
  _childInds :: Lens' s a


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
instance HasParentInds TreeContext ParentIndex where
  _parentInds =  lens parentIndsT (\l c -> l {parentIndsT = c})

instance HasParentInds (TreeIndexData e) ParentIndex where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds TreeContext (Pair ChildIndex ChildIndex) where
  _childInds = lens childIndsT (\l c -> l {childIndsT = c})

instance HasChildInds (TreeIndexData e) (Pair ChildIndex ChildIndex) where
  _childInds = _nodeContext . _childInds


instance HasLeft (TreeIndexData e) ChildIndex where
  _left = _childInds . _left

instance HasRight (TreeIndexData e) ChildIndex where
  _right = _childInds . _right


--      ┌────────────────────────┐
--      │    Network Accessors   │
--      └────────────────────────┘
instance HasParentInds NetworkContext (Pair ParentIndex ParentIndex) where
  _parentInds = lens parentIndsN (\l c -> l {parentIndsN = c})

instance HasParentInds (NetworkIndexData e) (Pair ParentIndex ParentIndex) where
  _parentInds = _nodeContext . _parentInds

instance HasChildInds NetworkContext ChildIndex where
  _childInds = lens childIndsN (\l c -> l {childIndsN = c})

instance HasChildInds (NetworkIndexData e) ChildIndex where
  _childInds = _nodeContext . _childInds


--      ┌─────────────────────┐
--      │    Root Accessors   │
--      └─────────────────────┘

instance HasChildInds RootContext (Either ChildIndex (Pair ChildIndex ChildIndex)) where
  _childInds = lens childIndsR (\l c -> l {childIndsR = c})

instance HasChildInds (RootIndexData e) (Either ChildIndex (Pair ChildIndex ChildIndex)) where
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
