{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.EdgeAware.Class where

-- | An edge aware tree can use the tree context to get the edges of a node
class EdgedTree t n e | t -> e where
  edges :: n -> t -> e
  setEdges :: n -> t -> e -> t