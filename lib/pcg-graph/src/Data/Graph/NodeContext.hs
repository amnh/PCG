{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Data.Graph.NodeContext where

import Data.Graph.Indices

data Two a = Single !a | Pair !a !a

data NodeContext
  = NodeContext {-# UNPACK #-} !(Two ParentIndex) {-# UNPACK #-} !(Two ChildIndex)

data IndexData n = IndexData
  { indexData :: n
  , nodeContext :: !NodeContext
  }


newtype RootContext = RootContext { childInds :: Two ChildIndex}
data RootIndexData r = RootIndexData
  { rootIndexData :: r
  , rootContext   :: !RootContext
  }

newtype LeafContext = LeafContext { parentInds :: ParentIndex }
data LeafIndexData t = LeafIndexData
  { leafIndexData :: t
  , leafContext   :: !LeafContext
  }

data NetworkContext
  = NetworkContext {-# UNPACK #-} !(Two ParentIndex) {-# UNPACK #-} !ChildIndex

data NetworkIndexData n = NetworkIndexData
  { networkIndexData :: n
  , networkContext   :: !NetworkContext
  }

data InternalContext
  = InternalContext {-# UNPACK #-} !ParentIndex {-# UNPACK #-} !(Two ChildIndex)

data InternalIndexData i = InternalIndexData
  { internalIndexData :: i
  , internalContext   :: !InternalContext
  }

