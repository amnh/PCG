{-# LANGUAGE FunctionalDependencies #-}

module Data.Tree.Node.EdgeAware where

class EdgedNode a e | a -> e where
  edges :: a -> [e]
  setEdges :: a -> [e] -> a