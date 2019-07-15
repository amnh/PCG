{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.Graph.Indices where

import Data.Monoid
import Data.Bits
import Data.Word

-- Note: GHC-8.10 will allow UnliftedNewtypes at which point
-- it may be better to instead use:
-- newtype Ind = Ind {getInd :: Word#}
newtype LeafInd = LeafInd {getLeafInd     :: Word64}
  deriving stock  (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype RootInd = RootInd {getRootInd     :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype NetworkInd = NetworkInd {getNetworkInd  :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)

newtype InternalInd = InternalInd {getInternalInd :: Word64}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)


data Ind = L LeafInd | R RootInd | N NetworkInd | I InternalInd
data ChildInd = CL LeafInd | CN NetworkInd | CI InternalInd
data ParentInd = PR RootInd | PN NetworkInd | PI InternalInd


-- Tag Info:
--       |      Tag        |    IndexType    |
--       |  bit63, bit62   |
--       |     0 , 0       |    Leaf         |
--       |     1 , 0       |    Internal     |
--       |     0 , 1       |    Network      |
--       |     1 , 1       |    Root         |

newtype TaggedIndex  = TaggedIndex {getIndex :: Int }
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype Bits

newtype ParentIndex  = ParentIndex {getParentIndex :: Int}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype Bits

newtype ChildIndex   = ChildIndex  {getChildIndex :: Int}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (Enum, Bits)

data IndexType = Leaf | Internal | Network | Root
  deriving Enum


tagValue :: IndexType -> Int -> TaggedIndex
tagValue ty i = TaggedIndex (i .|. fromEnum ty `shiftL` 61)


getTag :: TaggedIndex -> IndexType
getTag = toEnum . getIndex . (`shiftR` 61)

untagValue :: TaggedIndex -> Int
untagValue (TaggedIndex i) = clearBit (clearBit i 62) 61
