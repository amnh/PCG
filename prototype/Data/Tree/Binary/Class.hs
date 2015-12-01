{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Tree.Binary.Class where

class BinaryTree t n where
  parent                :: n -> t -> Maybe n
  leftChild, rightChild :: n -> t -> Maybe n
  bothChildren          :: n -> t -> (Maybe n, Maybe n)
  leftChild    x t = fst $ bothChildren x t
  rightChild   x t = snd $ bothChildren x t
  bothChildren x t = (leftChild x t, rightChild x t)
