-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Tree
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Upper-level wrapper for all the lovely tree typeclasses
--
-----------------------------------------------------------------------------

module Bio.PhyloGraph.Tree
  ( module X
  ) where

import Bio.PhyloGraph.Tree.Binary      as X
import Bio.PhyloGraph.Tree.EdgeAware   as X
import Bio.PhyloGraph.Tree.Parsed      as X
import Bio.PhyloGraph.Tree.Referential as X
import Bio.PhyloGraph.Tree.Rose        as X
