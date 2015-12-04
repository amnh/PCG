{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Tree.Network.Class where

class Network t n | t -> n where
    parents :: n -> t -> Maybe [n] -- use an empty list instead
    children :: n -> t -> Maybe [n]
    root :: t -> n
    isLeaf :: n -> t -> Bool
    isRoot :: n -> t -> Bool
    update :: t -> [n] -> t