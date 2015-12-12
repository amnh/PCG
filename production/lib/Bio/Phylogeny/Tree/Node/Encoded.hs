{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Encoded where

import Bio.Sequence.Coded

-- | An encoded node allows getting and setting on encoded data
class EncodedNode a b | a -> b where
  encoded :: a -> EncodedSeq b
  setEncoded :: a -> EncodedSeq b -> a
