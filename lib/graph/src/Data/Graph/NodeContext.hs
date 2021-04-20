------------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.NodeContext
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Graph.NodeContext
--  where
  ( -- LabelledChildIndex
    Direction(..)
  , IndexData
  , NodeIndexData(..)
  , RootContext
  , RootIndexData
  , LeafContext
  , LeafIndexData
  , NetworkContext
  , NetworkIndexData
  , TreeContext
  , TreeIndexData
--  , Reindexable
  , HasParentInds(..)
  , HasChildInfo(..)
  , HasChildInds(..)
  , HasLeftChildIndex(..)
  , HasRightChildIndex(..)
  , HasLeftChildInfo(..)
  , HasRightChildInfo(..)
  , HasNodeData(..)
  , HasNodeContext(..)
  , HasLiftedNodeData(..)
  , rootIndexData
  , leafIndexData
  , networkIndexData
  , treeIndexData
  , _getChildLens
  , _getOtherChildLens
  , writeNodeData
  , modifyNodeData
  , liftFunction
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Graph.Indices
import           Data.Pair.Strict
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV


--      ┌──────────────────────────────┐
--      │    Polymorphic Index Data    │
--      └──────────────────────────────┘


{-
data  LabelledChildIndex e
    = LabelledChildIndex
    { edgeAnnotation     :: e
    , labelledChildIndex :: {-# UNPACK #-} !ChildIndex
    }
-}


-- |
-- Datum stored at a vector index. Generalized for each node type.
data  IndexData nodeContext nodeData
    = IndexData
    { nodeData    :: nodeData
    , nodeContext :: !nodeContext
    }
    deriving stock (Eq, Functor, Show)


--      ┌──────────────────────────┐
--      │    Combined Index Data   │
--      └──────────────────────────┘


-- |
-- Datum stored at a specific vector index. Specialized for each node type.
data  NodeIndexData d e l
    = LeafNodeIndexData     (LeafIndexData l)
    | RootNodeIndexData     (RootIndexData d e)
    | NetworkNodeIndexData  (NetworkIndexData d e)
    | TreeNodeIndexData     (TreeIndexData d e)


--      ┌──────────────────────────────┐
--      │    Node Contexts and Data    │
--      └──────────────────────────────┘


-- |
-- The context of a /root/ node.
newtype RootContext e = RootContext
    { childInfoR :: Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)
    }
    deriving stock Show


-- |
-- Synonym for the index data of a /root/ node.
type RootIndexData d e = IndexData (RootContext e) d


-- |
-- The context of a /leaf/ node.
newtype LeafContext = LeafContext
    { parentIndsL :: ParentIndex
    }
    deriving stock (Show)


-- |
-- Synonym for the index data of a /leaf/ node.
type LeafIndexData d = IndexData LeafContext d


-- |
-- The context of a /network/ node.
data NetworkContext e = NetworkContext
    { parentIndsN :: {-# UNPACK #-} !(ParentIndex :!: ParentIndex)
    , childInfoN  :: {-# UNPACK #-} !(ChildInfo e)
    }
    deriving stock Show


-- |
-- Synonym for the index data of a /network/ node.
type NetworkIndexData d e = IndexData (NetworkContext e) d


-- |
-- The context of a /tree/ node.
data TreeContext e = TreeContext
    { parentIndsT :: {-# UNPACK #-} !ParentIndex
    , childInfoT  :: {-# UNPACK #-} !(ChildInfo e :!: ChildInfo e)
    }
    deriving stock (Show)


-- |
-- Synonym for the index data of a /tree/ node.
type TreeIndexData d e = IndexData (TreeContext e) d


-- |
-- This is for use to say which child we are looking at
data Direction = L | R


--      ┌──────────────────────────────────┐
--      │    Type classes for reindexing   │
--      └──────────────────────────────────┘


{-
class Reindexable s where

  increment :: s -> s

  decrement :: s -> s
-}


--      ┌─────────────────────┐
--      │    Classy Lenses    │
--      └─────────────────────┘


-- |
-- A 'Control.Lens.Type.Lens' for the @parentInds@ field.
class HasParentInds s a | s -> a where

    _parentInds :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the @childInfo@ field.
class HasChildInfo s a | s -> a where

    _childInfo :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the @childInds@ field.
class HasChildInds s a | s -> a where

    _childInds :: Getter s a


-- |
-- A 'Control.Lens.Type.Lens' for the @leftChildIndex@ field.
class HasLeftChildIndex s a | s -> a where

    _leftChildIndex :: Getter s a


-- |
-- A 'Control.Lens.Type.Lens' for the @rightChildIndex@ field.
class HasRightChildIndex s a | s -> a where

    _rightChildIndex :: Getter s a


-- |
-- A 'Control.Lens.Type.Lens' for the @leftChildInfo@ field.
class HasLeftChildInfo s a | s -> a where

    _leftChildInfo :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the @rightChildInfo@ field.
class HasRightChildInfo s a | s -> a where

    _rightChildInfo :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the @nodeData@ field.
class HasNodeData s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _nodeData :: Lens s t a b


-- |
-- A 'Control.Lens.Type.Lens' for the @nodeContext@ field.
class HasNodeContext s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _nodeContext :: Lens s t a b


-- |
-- A 'Control.Lens.Type.Lens' for the @liftedNodeData@ field.
class HasLiftedNodeData s a where

    _liftedNodeData :: Getter s a


instance Applicative f => HasLiftedNodeData (NodeIndexData (f n) e n) (f n) where

    _liftedNodeData = to $
        \case
           LeafNodeIndexData    ind -> pure $ ind ^. _nodeData
           RootNodeIndexData    ind -> ind ^. _nodeData
           NetworkNodeIndexData ind -> ind ^. _nodeData
           TreeNodeIndexData    ind -> ind ^. _nodeData


instance Bifunctor IndexData where

    bimap f g (IndexData n d) =
      IndexData
      { nodeData = g n
      , nodeContext = f d
      }


instance HasNodeContext (IndexData c d) (IndexData c' d) c c'  where

    _nodeContext = lens nodeContext (\ind context -> ind { nodeContext = context })


instance HasNodeData (IndexData c d) (IndexData c d') d d' where

    _nodeData = lens nodeData (\ind datum -> ind { nodeData = datum })


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


instance HasChildInfo (TreeContext e) (ChildInfo e :!: ChildInfo e) where

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


instance HasChildInds (NetworkContext e) ChildIndex where

    _childInds = _childInfo . _childIndex


instance HasChildInfo (NetworkIndexData d e) (ChildInfo e) where

    _childInfo = _nodeContext . _childInfo


instance HasChildInds (NetworkIndexData d e) ChildIndex where

    _childInds = _childInfo . _childIndex


--      ┌─────────────────────┐
--      │    Root Accessors   │
--      └─────────────────────┘

instance HasChildInfo (RootContext e)
                      (Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)) where
    _childInfo = lens childInfoR (\l c -> l {childInfoR = c})


instance HasChildInds (RootContext e) (Either ChildIndex (ChildIndex :!: ChildIndex)) where

    _childInds =
      let
        getChildInds
          :: Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)
          -> Either ChildIndex    (ChildIndex :!: ChildIndex)
        getChildInds = bimap (view _childIndex) (view (_both _childIndex))
        _view :: RootContext e -> Either ChildIndex (ChildIndex :!: ChildIndex)
        _view = getChildInds . view _childInfo
      in
        to _view


instance HasChildInfo (RootIndexData d e)
                      (Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)) where

    _childInfo = _nodeContext . _childInfo


instance HasChildInds (RootIndexData d e)
                      (Either ChildIndex (ChildIndex :!: ChildIndex)) where

    _childInds = _nodeContext . _childInds


-- |
-- Smart constructor of 'LeafIndexData'.
leafIndexData :: d -> ParentIndex -> LeafIndexData d
leafIndexData d ind =
  IndexData
  { nodeData = d
  , nodeContext = LeafContext ind
  }


-- |
-- Smart constructor of 'networkIndexData'.
networkIndexData
  :: d
  -> ParentIndex :!: ParentIndex
  -> ChildInfo e
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


-- |
-- Smart constructor of 'rootIndexData'.
rootIndexData
  :: d
  -> Either (ChildInfo e) (ChildInfo e :!: ChildInfo e)
  -> RootIndexData d e
rootIndexData d inds =
  IndexData
  { nodeData = d
  , nodeContext = RootContext inds
  }


-- |
-- Smart constructor of 'treeIndexData'.
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
-- Extract the child accessor 'Control.Lens.Type.Lens' which /corresponds to/ the supplied direction.
_getChildLens :: (HasLeft s a, HasRight s a) => Direction -> Lens' s a
_getChildLens =
    \case
      L -> _left
      R -> _right


-- |
-- Extract the child accessor 'Control.Lens.Type.Lens' which /is opposite to/ the supplied direction.
_getOtherChildLens :: (HasLeft s a, HasRight s a) => Direction -> Lens' s a
_getOtherChildLens =
    \case
      L -> _right
      R -> _left


-- |
-- Write the supplied value at the specified index of the 'Vector'.
writeNodeData :: HasNodeData s s a a => Int -> a -> Vector s -> Vector s
writeNodeData i a =
    V.modify $ \mv -> MV.modify mv (& _nodeData .~ a) i


-- |
-- Mutate the value at the specified index of the 'Vector'.
modifyNodeData :: HasNodeData s s a a => Int -> (a -> a) -> Vector s -> Vector s
modifyNodeData i fn =
    V.modify $ \mv -> MV.modify mv (& _nodeData %~ fn) i


-- |
-- Lift a binary function over a 'NodeIndexData'.
liftFunction
  :: (Applicative f)
  => (n -> n -> n)
  -> (NodeIndexData (f n) e n -> NodeIndexData (f n) e n -> f n)
liftFunction fn nInd1 nInd2 =
    liftA2 fn (nInd1 ^. _liftedNodeData) (nInd2 ^. _liftedNodeData)
