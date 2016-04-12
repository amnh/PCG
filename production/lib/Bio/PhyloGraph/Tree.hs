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

module Bio.PhyloGraph.Tree   ( module Bio.PhyloGraph.Tree.Binary
                            , module Bio.PhyloGraph.Tree.EdgeAware
                            , module Bio.PhyloGraph.Tree.Parsed
                            , module Bio.PhyloGraph.Tree.Referential
                            , module Bio.PhyloGraph.Tree.Rose) where

import Bio.PhyloGraph.Tree.Binary
import Bio.PhyloGraph.Tree.EdgeAware
import Bio.PhyloGraph.Tree.Parsed
import Bio.PhyloGraph.Tree.Referential
import Bio.PhyloGraph.Tree.Rose