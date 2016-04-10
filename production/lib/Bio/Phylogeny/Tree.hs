-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Tree
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

module Bio.Phylogeny.Tree   ( module Bio.Phylogeny.Tree.Binary
                            , module Bio.Phylogeny.Tree.EdgeAware
                            , module Bio.Phylogeny.Tree.Parsed
                            , module Bio.Phylogeny.Tree.Referential
                            , module Bio.Phylogeny.Tree.Rose) where

import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Tree.EdgeAware
import Bio.Phylogeny.Tree.Parsed
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Rose