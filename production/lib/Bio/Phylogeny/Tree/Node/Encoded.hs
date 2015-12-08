{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Encoded where

import Data.Bits
import Data.Int
import Data.Vector
import Data.Sequence.Encoded

class EncodedNode a b | a -> b where
  encoded :: a -> EncodedSeq
  setEncoded :: a -> EncodedSeq -> a
