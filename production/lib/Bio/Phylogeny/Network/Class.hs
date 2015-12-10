{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.Phylogeny.Network.Class where

class Network t n | t -> n where
    parents :: n -> t -> [n] 
    children :: n -> t -> [n]
    root :: t -> n
    isLeaf :: n -> t -> Bool
    isRoot :: n -> t -> Bool
    update :: t -> [n] -> t