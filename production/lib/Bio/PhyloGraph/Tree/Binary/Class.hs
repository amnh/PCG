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
-- Class for a binary tree that must also be a rose tree
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.PhyloGraph.Tree.Binary.Class where

import Bio.PhyloGraph.Tree.Rose

-- | A binary tree is a type of rose tree where you can get left and right children
class RoseTree t n => BinaryTree t n | t -> n where
  leftChild, rightChild :: n -> t -> Maybe n
  bothChildren          :: n -> t -> (Maybe n, Maybe n)
  bothChildren x t = (leftChild x t, rightChild x t)
  leftChild    x t = fst $ bothChildren x t
  rightChild   x t = snd $ bothChildren x t
  verifyBinary :: t -> Bool

