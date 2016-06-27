-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Forest.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class expressing the requirements for a forest, notated as @f@
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Bio.PhyloGraph.Forest.Class where

import Bio.PhyloGraph.Forest.Internal

-- TODO: Add restrictions for a valid forest

-- | A forest is simply a list of trees that can be filtered or set.
-- change to a more generic array type (foldable functor etc)
class GeneralForest f t | f -> t where
    trees       :: f -> [t]
    setTrees    :: f -> [t] -> f
    filterTrees :: f -> (t -> Bool) -> f
    -- transfer subtree

-- | (✔)
instance GeneralForest (Forest d) d where
    trees                  = id
    setTrees         _ new = new
    filterTrees forest f   = filter f forest
                                                      
