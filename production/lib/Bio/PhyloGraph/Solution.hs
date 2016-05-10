-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Instances and other stuff for a solution representation
--
-----------------------------------------------------------------------------

module Bio.PhyloGraph.Solution
  ( GeneralSolution (..)
  , Identifier
  , MetadataSolution(..)
  , SearchState
  , Sequences
  , Solution        (..)
  , StandardMetadata
  , StandardSolution
  ) where

import Bio.PhyloGraph.Solution.Class
import Bio.PhyloGraph.Solution.Internal
import Bio.PhyloGraph.Solution.Metadata
