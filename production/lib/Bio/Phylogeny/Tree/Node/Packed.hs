{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Packed where

import Bio.Sequence.Packed

-- | A packed node has its packed data
class PackedNode a c | a -> c where
    packed :: a -> c
    setPacked :: a -> c -> a