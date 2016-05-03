-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Forest.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Forest representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Forest.Internal where

import Bio.PhyloGraph.Forest.Class

-- | A forest is a list of dag structures where dags can be referential or
--   topological.
type Forest d = [d]

instance GeneralForest (Forest d) d where
    trees = id
    setTrees _ new = new
    filterTrees forest f = filter f forest