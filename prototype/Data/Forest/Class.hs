{-# LANGUAGE FunctionalDependencies #-}

module Data.Forest.Class where

class Forest f t | t -> f where
    trees :: f -> [t]
    setTrees :: f -> [t] -> f
    filterTrees :: f -> (t -> Bool) -> f