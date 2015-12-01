{-# LANGUAGE FunctionalDependencies #-}

module Data.Tree.Node.Final where

import Data.Sequence.Coded

class FinalNode a b | a -> b where
    final :: a -> EncodedSeq b
    setFinal :: EncodedSeq b -> a -> a