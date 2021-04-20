------------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Indices
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Data.Graph.Indices
  ( ChildInfo(..)
  , ChildIndex(..)
  , HasChildIndex(..)
  , IndexType(..)
  , ParentIndex(..)
  , TaggedIndex(..)
  , UntaggedIndex
  -- * Tagged indices
  , LeafInd(..)
  , NetworkInd(..)
  , RootInd(..)
  , TreeInd(..)
  -- * Classes
  , Tagged(..)
  -- * Conversion
  , childInfo
  , toUntagged
  ) where

import Control.Lens
import Data.Monoid
import Data.Pair.Strict


-- |
-- The child index of a node and the datum of the edge connecting the node to
-- the child.
data  ChildInfo e
    = ChildInfo
    { childIndex :: ChildIndex
    , edgeData   :: e
    }
    deriving stock (Eq, Show, Functor)


{-
--      ┌───────────────────────┐
--      │    Edge Index Data    │
--      └───────────────────────┘

data  EdgeIndex
    = EdgeIndex
    { edgeParType    :: IndexType
    , edgeParIndex   :: ParentIndex
    , edgeChildIndex :: ChildIndex
    }
-}


-- |
-- Different tag types for node indices.
data IndexType = LeafTag | TreeTag | NetworkTag | RootTag
    deriving stock (Eq, Enum)


-- |
-- An index tagged by the node type and the index of the ocrresponding vector.
data  TaggedIndex
    = TaggedIndex
    { untaggedIndex :: {-# UNPACK #-} !Int
    , tag           :: !IndexType
    }
    deriving stock (Eq, Show)


-- |
-- A wrapper denoting that the tagged index is the parent of a node.
newtype ParentIndex  = ParentIndex {getParentIndex :: TaggedIndex}
    deriving stock   (Eq, Show)
    deriving newtype (Tagged)


-- |
-- A wrapper denoting that the tagged index is the child of a node.
newtype ChildIndex   = ChildIndex  {getChildIndex :: TaggedIndex}
    deriving stock   (Eq, Show)
    deriving newtype (Tagged)


-- |
-- A wrapper denoting that the index is of a /leaf/ node.
newtype LeafInd = LeafInd {getLeafInd :: Int}
    deriving stock  (Eq, Ord, Show)
    deriving (Semigroup, Monoid) via (Sum Int)


-- |
-- A wrapper denoting that the index is of a /network/ node.
newtype NetworkInd = NetworkInd {getNetworkInd  :: Int}
    deriving stock (Eq, Ord, Show)
    deriving (Semigroup, Monoid) via (Sum Int)


-- |
-- A wrapper denoting that the index is of a /tree/ node.
newtype TreeInd = TreeInd {getTreeInd :: Int}
    deriving stock (Eq, Ord, Show)
    deriving (Semigroup, Monoid) via (Sum Int)


-- |
-- A wrapper denoting that the index is of a /root/ node.
newtype RootInd = RootInd {getRootInd :: Int}
    deriving stock (Eq, Ord, Show)
    deriving (Semigroup, Monoid) via (Sum Int)


-- |
-- A synonym for an index without a congextual tag.
type UntaggedIndex = Int


-- |
-- A 'Control.Lens.Type.Len' for the 'childIndex' field.
class HasChildIndex s a | s -> a where

    _childIndex :: Lens' s a


-- |
-- A 'Control.Lens.Type.Len' for the 'edgeData' field.
class HasEdgeData s t a b | s -> a, t -> b, s b -> t, t a -> s where

    _edgeData :: Lens s t a b


-- |
-- Class for interacting with tagged indices.
class Tagged t where

    tagValue   :: IndexType -> Int -> t

    getTag     :: t -> IndexType

    getIndex   :: t -> Int


instance Tagged TaggedIndex where

    tagValue :: IndexType -> Int -> TaggedIndex
    tagValue ty i = TaggedIndex i ty

    getTag :: TaggedIndex -> IndexType
    getTag = tag

    getIndex :: TaggedIndex -> Int
    getIndex = untaggedIndex


instance HasChildIndex (ChildInfo e) ChildIndex where

    _childIndex = lens childIndex (\c i -> c { childIndex = i})


instance HasEdgeData (ChildInfo e) (ChildInfo e') e e' where

    _edgeData = lens edgeData (\c e -> c { edgeData = e})


instance Show IndexType where
  show =
    \case
      LeafTag    -> "L"
      TreeTag    -> "T"
      NetworkTag -> "N"
      RootTag    -> "R"


-- |
-- Smart constructor for 'ChildInfo'.
childInfo :: IndexType -> Int -> e -> ChildInfo e
childInfo indtype n e =
    ChildInfo
    { childIndex = tagValue indtype n
    , edgeData   = e
    }


-- |
-- Deconstructor for a 'Tagged' index.
toUntagged :: (Tagged t) => t -> Pair IndexType UntaggedIndex
toUntagged ind = getTag ind :!: getIndex ind
