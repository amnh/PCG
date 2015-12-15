{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.Final where

import Data.Vector

-- | A final node has its final assignment
class FinalNode n s | n -> s where
    final :: n -> Vector s
    setFinal :: Vector s -> n -> n