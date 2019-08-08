{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}


module Data.Graph.Indices where

import Data.Monoid
import Data.Bits
import Data.Word


newtype LeafInd = LeafInd {getLeafInd     :: Word64}
  deriving stock  (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype RootInd = RootInd {getRootInd     :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype NetworkInd = NetworkInd {getNetworkInd  :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype TreeInd = TreeInd {getTreeInd :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)


data Ind = L LeafInd | R RootInd | N NetworkInd | I TreeInd
data ChildInd = CL LeafInd | CN NetworkInd | CI TreeInd
data ParentInd = PR RootInd | PN NetworkInd | PI TreeInd

class Tagged t where
  tagValue   :: IndexType -> Int -> t
  getTag     :: t -> IndexType
  untagValue :: t -> Int


-- Tag Info:
--       |      Tag        |    IndexType    |
--       |  bit63, bit62   |
--       |     0 , 0       |    Leaf         |
--       |     1 , 0       |    Tree     |
--       |     0 , 1       |    Network      |
--       |     1 , 1       |    Root         |

newtype TaggedIndex  = TaggedIndex {getIndex :: Int }
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (Bits, Num)

instance Tagged TaggedIndex where
  tagValue :: IndexType -> Int -> TaggedIndex
  tagValue ty i = TaggedIndex (i .|. fromEnum ty `shiftL` 61)

  getTag :: TaggedIndex -> IndexType
  getTag = toEnum . getIndex . (`shiftR` 61)

  untagValue :: TaggedIndex -> Int
  untagValue (TaggedIndex i) = clearBit (clearBit i 62) 61
    

newtype ParentIndex  = ParentIndex {getParentIndex :: TaggedIndex}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (Bits, Tagged, Num)

newtype ChildIndex   = ChildIndex  {getChildIndex :: TaggedIndex}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (Bits, Tagged, Num)

data IndexType = LeafTag | TreeTag | NetworkTag | RootTag
  deriving stock (Eq, Enum)



