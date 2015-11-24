module Data.Tree.Node.Encoded where

import Data.Bits
import Data.Int
import Data.Vector

class EncodedNode a where
  encoded :: a -> Maybe (Vector Int64)
