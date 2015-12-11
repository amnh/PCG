{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.EdgeAware where

-- | An edge aware node can get a list of its edges
class EdgedNode a e | a -> e where
  edges :: a -> [e]
  setEdges :: a -> [e] -> a