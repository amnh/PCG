{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}


module Data.Graph.Indices where

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


-- Tag Info:
--       |      Tag        |    IndexType    |
--       |  bit63, bit62   |
--       |     0 , 0       |    Leaf         |
--       |     1 , 0       |    Tree     |
--       |     0 , 1       |    Network      |
--       |     1 , 1       |    Root         |

data TaggedIndex  = TaggedIndex
  { untaggedIndex :: {-# UNPACK #-} !Int
  , tag      :: {-# UNPACK #-} !IndexType
  }

  deriving stock (Eq, Show)
--  deriving (Semigroup, Monoid) via (Sum Int)
--  deriving newtype (Bits, Num)

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
