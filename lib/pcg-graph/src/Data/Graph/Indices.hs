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


module Data.Graph.Indices where

import Control.Lens
import Data.Monoid
import Data.Pair.Strict


newtype LeafInd = LeafInd {getLeafInd :: Int}
  deriving stock  (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype RootInd = RootInd {getRootInd :: Int}
  deriving stock (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype NetworkInd = NetworkInd {getNetworkInd  :: Int}
  deriving stock (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype TreeInd = TreeInd {getTreeInd :: Int}
  deriving stock (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

class Tagged t where
  tagValue   :: IndexType -> Int -> t
  getTag     :: t -> IndexType
  getIndex   :: t -> Int


data TaggedIndex  = TaggedIndex
  { untaggedIndex :: {-# UNPACK #-} !Int
  , tag           :: !IndexType
  }

  deriving stock (Eq, Show)

type UntaggedIndex = Int

instance Tagged TaggedIndex where
  tagValue :: IndexType -> Int -> TaggedIndex
  tagValue ty i = TaggedIndex i ty

  getTag :: TaggedIndex -> IndexType
  getTag = tag

  getIndex :: TaggedIndex -> Int
  getIndex = untaggedIndex


newtype ParentIndex  = ParentIndex {getParentIndex :: TaggedIndex}
  deriving stock (Eq, Show)
  deriving newtype (Tagged)


newtype ChildIndex   = ChildIndex  {getChildIndex :: TaggedIndex}
  deriving stock (Eq, Show)
  deriving newtype (Tagged)

data ChildInfo e =
  ChildInfo
  { childIndex :: ChildIndex
  , edgeData   :: e
  }
  deriving stock (Eq, Show, Functor)

childInfo :: IndexType -> Int -> e -> ChildInfo e
childInfo indtype n e =
  ChildInfo
  { childIndex = tagValue indtype n
  , edgeData   = e
  }

class HasChildIndex s a | s -> a where
  _childIndex :: Lens' s a

instance HasChildIndex (ChildInfo e) ChildIndex where
  _childIndex = lens childIndex (\c i -> c { childIndex = i})

class HasEdgeData s t a b| s -> a, t -> b, s b -> t, t a -> s where
  _edgeData :: Lens s t a b

instance HasEdgeData (ChildInfo e) (ChildInfo e') e e' where
  _edgeData = lens edgeData (\c e -> c { edgeData = e})



data IndexType = LeafTag | TreeTag | NetworkTag | RootTag
  deriving stock (Eq, Enum)

instance Show IndexType where
  show =
    \case
      LeafTag    -> "L"
      TreeTag    -> "T"
      NetworkTag -> "N"
      RootTag    -> "R"


toUntagged :: (Tagged t) => t -> Pair IndexType UntaggedIndex
toUntagged ind = getTag ind :!: getIndex ind



--      ┌───────────────────────┐
--      │    Edge Index Data    │
--      └───────────────────────┘

data EdgeIndex = EdgeIndex
  { edgeParType    :: IndexType
  , edgeParIndex   :: ParentIndex
  , edgeChildIndex :: ChildIndex
  }
