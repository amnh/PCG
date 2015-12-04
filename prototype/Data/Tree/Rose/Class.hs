{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Tree.Rose.Class where

import Data.Tree.Network 
import Control.Monad
import Safe

class Network t n => RoseTree t n | t -> n where
  parent :: n -> t -> Maybe n
  parent n t = join $ headMay <$> (parents n t)