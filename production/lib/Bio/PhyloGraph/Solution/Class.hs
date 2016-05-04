-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard outline for a Solution to allow typeclass restrictions
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.PhyloGraph.Solution.Class where
-- TODO: Add validity checks

-- | Accessor and mutator for types which store a collection of 'Forest'.
class GeneralSolution s f | s -> f where
    getForests :: s -> [f]
    setForests :: s -> [f] -> s
