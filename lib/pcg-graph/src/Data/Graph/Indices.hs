{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}


module Data.Graph.Indices where

import Control.Lens
import Data.Monoid
import Data.Pair.Strict
import Data.Coerce
import Data.Hashable
import Data.Bits
import           GHC.Generics         (Generic)
import Control.DeepSeq


-- To do: put this into pcg-utility
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x


newtype LeafInd = LeafInd {getLeafInd :: Int}
  deriving stock  (Eq, Ord, Generic, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (NFData)

newtype RootInd = RootInd {getRootInd :: Int}
  deriving stock (Eq, Ord, Generic, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (NFData)

newtype NetworkInd = NetworkInd {getNetworkInd  :: Int}
  deriving stock (Eq, Ord, Generic, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (NFData)

newtype TreeInd = TreeInd {getTreeInd :: Int}
  deriving stock (Eq, Ord, Generic, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (NFData)

class Tagged t where
  tagValue   :: IndexType -> Int -> t
  fromTagged :: TaggedIndex -> t
  fromTagged (TaggedIndex ind tag) = tagValue tag ind
  getTag     :: t -> IndexType
  getIndex   :: t -> Int


data TaggedIndex  = TaggedIndex
  { untaggedIndex :: {-# UNPACK #-} !Int
  , tag           :: {-# UNPACK #-} !IndexType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)


instance Hashable TaggedIndex where
  hash (TaggedIndex uInd t)  = hash uInd `hashWithSalt` t
  hashWithSalt = defaultHashWithSalt

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
  { childIndex :: {-# UNPACK #-} !ChildIndex
  , edgeData   :: {-# UNPACK #-} !e
  }
  deriving stock (Eq, Show, Functor)

childInfo :: IndexType -> Int -> e -> ChildInfo e
childInfo indtype n e =
  ChildInfo
  { childIndex = tagValue indtype n
  , edgeData   = e
  }

childInfoTag :: TaggedIndex -> e -> ChildInfo e
childInfoTag tag e =
  ChildInfo
  { childIndex = coerce tag
  , edgeData   = e
  }


class HasChildIndex s a | s -> a where
  _childIndex :: Lens' s a


class HasIndexType s a | s -> a where
  _indexType :: Getter s a



instance HasIndexType TaggedIndex IndexType where
  _indexType = to tag


instance HasIndexType ParentIndex IndexType where
  _indexType = to (coerce tag)


instance HasIndexType ChildIndex IndexType where
  _indexType = to (coerce tag)


instance HasIndexType (ChildInfo e) IndexType where
  _indexType = _childIndex . _indexType


class HasUntaggedIndex s a | s -> a where
  _untaggedIndex :: Getter s a


instance HasUntaggedIndex TaggedIndex Int where
  _untaggedIndex = to untaggedIndex



instance HasChildIndex (ChildInfo e) ChildIndex where
  _childIndex = lens childIndex (\c i -> c { childIndex = i})

class HasEdgeData s t a b| s -> a, t -> b, s b -> t, t a -> s where
  _edgeData :: Lens s t a b

instance HasEdgeData (ChildInfo e) (ChildInfo e') e e' where
  _edgeData = lens edgeData (\c e -> c { edgeData = e})



data IndexType = LeafTag | TreeTag | NetworkTag | RootTag
  deriving stock (Eq, Ord, Enum, Generic)
  deriving anyclass (NFData)


instance Hashable IndexType where
  hash = fromEnum
  hashWithSalt = defaultHashWithSalt

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
  { edgeSource    :: {-# UNPACK #-} !TaggedIndex
  , edgeTarget    :: {-# UNPACK #-} !TaggedIndex
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

class HasEdgeSource s a | s -> a where
  _edgeSource :: Lens' s a

instance HasEdgeSource EdgeIndex TaggedIndex where
  _edgeSource = lens edgeSource (\e s -> e { edgeSource = s})

class HasEdgeTarget s a | s -> a where
  _edgeTarget :: Lens' s a

instance HasEdgeTarget EdgeIndex TaggedIndex where
  _edgeTarget = lens edgeTarget (\e s -> e { edgeTarget = s})

-- Adapted from the Hashable library
instance Hashable EdgeIndex where
  hash (EdgeIndex src tgt)  = hash src `hashWithSalt` tgt
  hashWithSalt = defaultHashWithSalt


data DirEdgeIndex = DirEdgeIndex
  { edgeIndex    :: EdgeIndex
  , reversedEdge :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)


class HasEdgeIndex s a | s -> a where
  _edgeIndex :: Lens' s a

instance HasEdgeIndex DirEdgeIndex EdgeIndex where
  _edgeIndex =  lens edgeIndex (\e s -> e { edgeIndex = s})

instance HasEdgeSource DirEdgeIndex TaggedIndex where
  _edgeSource = _edgeIndex . _edgeSource

instance HasEdgeTarget DirEdgeIndex TaggedIndex where
  _edgeTarget = _edgeIndex . _edgeTarget

-- |
-- This function flips the source and target indices but
-- remembers the original incidence relation.
flipDirEdgeIndex :: DirEdgeIndex -> DirEdgeIndex
flipDirEdgeIndex DirEdgeIndex{..} = DirEdgeIndex newEdgeIndex (not reversedEdge)
  where
    newEdgeIndex = EdgeIndex { edgeSource = edgeTarget edgeIndex
                             , edgeTarget = edgeSource edgeIndex
                             }


-- |
-- This function re-orients a directed edge index to have the implied orientation.
dirToEdgeIndex :: DirEdgeIndex -> EdgeIndex
dirToEdgeIndex dir@(DirEdgeIndex edgeInds flipped) =
  if flipped
    then edgeIndex (flipDirEdgeIndex dir)
    else edgeInds
           
