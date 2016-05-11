-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for an edge aware tree that understands its edges
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Bio.PhyloGraph.Tree.EdgeAware.Class where

-- | An edge aware tree can use the tree context to get the edges of a node
class EdgedTree t n e | t -> e where
  edges :: n -> t -> e
  setEdges :: n -> t -> e -> t
