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

module Bio.PhyloGraph.Forest.Internal where

-- | A forest is a list of dag structures where dags can be referential or
--   topological.
type Forest d = [d]
