{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Final where

import Bio.Sequence.Coded

-- | A final node has its final assignment
class FinalNode a b | a -> b where
    final :: a -> EncodedSeq b
    setFinal :: EncodedSeq b -> a -> a