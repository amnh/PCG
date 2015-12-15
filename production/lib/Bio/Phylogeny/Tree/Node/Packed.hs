{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Packed where

import Data.Vector

-- | A packed node has its packed data
class PackedNode a s | a -> s where
    packed :: a -> Vector s
    setPacked :: a -> Vector s -> a