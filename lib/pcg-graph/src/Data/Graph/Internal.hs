{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Graph.Internal where

import Data.Graph.Indices
import Data.Graph.NodeContext
import Data.Vector (Vector)
import Data.Key
import Data.Vector.Instances ()
import Control.Lens
import Data.Bifunctor
import Data.Graph.Type


postorderFold
  :: (t     -> r)
  -> ((f n) -> r)
  -> ((f n) -> r -> r)
  -> ((f n) -> r -> r -> r)
  -> Graph f e c n t -> r
postorderFold = undefined
