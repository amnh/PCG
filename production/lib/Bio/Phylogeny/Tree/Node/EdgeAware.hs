-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for nodes that can access their connected edges
--
-----------------------------------------------------------------------------


{-# LANGUAGE FunctionalDependencies #-}

module Bio.Phylogeny.Tree.Node.EdgeAware where

-- | An edge aware node can get a list of its edges
class EdgedNode a e | a -> e where
  edges :: a -> [e]
  setEdges :: a -> [e] -> a