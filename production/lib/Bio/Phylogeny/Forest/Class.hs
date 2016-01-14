-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class expressing the requirements for a forest, notated as f
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Forest.Class where

-- | A forest is simply a list of trees that can be filtered or set.
-- change to a more generic array type (foldable functor etc)
class Forest f t | t -> f where
    trees :: f -> [t]
    setTrees :: f -> [t] -> f
    filterTrees :: f -> (t -> Bool) -> f
    -- transfer subtree 