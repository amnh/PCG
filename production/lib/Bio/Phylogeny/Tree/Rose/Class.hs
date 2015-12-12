{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.Phylogeny.Tree.Rose.Class where

import Bio.Phylogeny.Network 
import Safe

-- | A rose tree can get its single parent
class Network t n => RoseTree t n | t -> n where
  parent :: n -> t -> Maybe n
  parent n t = headMay (parents n t)
