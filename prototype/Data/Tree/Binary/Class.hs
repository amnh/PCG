{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Tree.Binary.Class where

import Data.Maybe

class BinaryTree t n | t -> n where
  parent                :: n -> t -> Maybe n
  leftChild, rightChild :: n -> t -> Maybe n
  bothChildren          :: n -> t -> (Maybe n, Maybe n)
  root                  :: t -> n
  isLeaf                :: n -> t -> Bool
  update                :: t -> [n] -> t
  leftChild    x t = fst $ bothChildren x t
  rightChild   x t = snd $ bothChildren x t
  bothChildren x t = (leftChild x t, rightChild x t)
  isLeaf       x t = (isNothing $ leftChild x t) && (isNothing $ leftChild x t)
