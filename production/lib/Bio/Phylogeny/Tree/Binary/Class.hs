{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.Phylogeny.Tree.Binary.Class where

import Bio.Phylogeny.Tree.Rose

class RoseTree t n => BinaryTree t n | t -> n where
  leftChild, rightChild, parent :: n -> t -> Maybe n
  bothChildren          :: n -> t -> (Maybe n, Maybe n)
  bothChildren x t = (leftChild x t, rightChild x t)
  leftChild    x t = fst $ bothChildren x t
  rightChild   x t = snd $ bothChildren x t

