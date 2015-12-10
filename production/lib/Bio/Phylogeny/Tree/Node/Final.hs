{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Final where

import Bio.Sequence.Coded

class FinalNode a b | a -> b where
    final :: a -> EncodedSeq b
    setFinal :: EncodedSeq b -> a -> a