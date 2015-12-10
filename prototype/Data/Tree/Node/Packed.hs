{-# LANGUAGE FunctionalDependencies #-}

import Data.Sequence.Packed

module Data.Tree.Node.Packed where

class PackedNode a b | a -> b where
    packed :: a -> PackedSeq
    setPacked :: a -> PackedSeq -> a