-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Solution.Class
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

module Bio.Phylogeny.Solution.Class where
-- TODO: Add validity checks

class Solution s f | s -> f where
    forests :: s -> [f]
    setForests :: s -> [f] -> s
