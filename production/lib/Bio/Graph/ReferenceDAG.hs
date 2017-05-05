-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.ReferenceDAG
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Graph.ReferenceDAG
  ( -- * Efficient access
    ReferenceDAG()
  , NodeRef()
  , nodeFoldMap
  , nodePostOrder
  , nodePreOrder
  , unfoldDAG
  ) where

import Bio.Graph.ReferenceDAG.Internal
