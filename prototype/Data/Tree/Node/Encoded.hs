{-# LANGUAGE FunctionalDependencies #-}

module Data.Tree.Node.Encoded where

import Data.Bits
import Data.Int
import Data.Vector

class EncodedNode a b | a -> b where
  encoded :: a -> Maybe (Vector (Vector b))
  setEncoded :: a -> Maybe (Vector (Vector b)) -> a
