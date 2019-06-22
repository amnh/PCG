-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Custom
  ( fromList'
  ) where

import qualified Control.Foldl as L
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

-- |
-- /O(n)/
--
-- Construct a 'Vector' from a list.
{-# INLINE fromList' #-}
fromList' :: [a] -> Vector a
fromList' = uncurry V.fromListN . L.fold f
  where
    f :: L.Fold a (Int, [a])
    f = (,) <$> L.length <*> L.list
