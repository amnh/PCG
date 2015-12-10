{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Encoded where

import Bio.Sequence.Coded

class EncodedNode a b | a -> b where
  encoded :: a -> EncodedSeq b
  setEncoded :: a -> EncodedSeq b -> a
