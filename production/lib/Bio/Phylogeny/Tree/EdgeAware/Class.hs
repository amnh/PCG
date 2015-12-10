{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.EdgeAware.Class where

class EdgedTree t n e | t -> e where
  edges :: n -> t -> e
  setEdges :: n -> t -> e -> t