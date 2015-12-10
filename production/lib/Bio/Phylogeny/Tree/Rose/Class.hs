{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Bio.Phylogeny.Tree.Rose.Class where

import Bio.Phylogeny.Network 
import Control.Monad
import Safe

class Network t n => RoseTree t n | t -> n where
  parent :: n -> t -> Maybe n
  parent n t = headMay (parents n t)