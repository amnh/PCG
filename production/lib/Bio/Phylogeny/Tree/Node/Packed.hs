{-# LANGUAGE FunctionalDependencies #-}

import Bio.Phylogeny.Sequence.Packed

module Bio.Phylogeny.Tree.Node.Packed where

class PackedNode a b | a -> b where
    packed :: a -> PackedSeq
    setPacked :: a -> PackedSeq -> a