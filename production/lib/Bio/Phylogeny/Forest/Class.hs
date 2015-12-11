{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Forest.Class where

-- | A forest is simply a list of trees that can be filtered or set
class Forest f t | t -> f where
    trees :: f -> [t]
    setTrees :: f -> [t] -> f
    filterTrees :: f -> (t -> Bool) -> f