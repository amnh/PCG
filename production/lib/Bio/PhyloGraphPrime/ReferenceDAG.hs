-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraphPrime.ReferenceDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.PhyloGraphPrime.ReferenceDAG
  ( -- * Efficient access
    ReferenceDAG()
  , NodeRef()
  , nodePostOrder
  , nodePreOrder
  , unfoldDAG
  ) where

import Bio.PhyloGraphPrime.ReferenceDAG.Internal
