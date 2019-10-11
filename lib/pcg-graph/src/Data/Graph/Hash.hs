{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Graph.Hash where

import Control.Lens

class HasHashValue s a | s -> a where
  _hashValue :: Lens' s a



