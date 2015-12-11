{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.Phylogeny.Network.Class where

-- | A network holds topological information as well as the ability to update based on a list of nodes
class Network t n | t -> n where
    parents :: n -> t -> [n] 
    children :: n -> t -> [n]
    root :: t -> n
    isLeaf :: n -> t -> Bool
    isRoot :: n -> t -> Bool
    update :: t -> [n] -> t