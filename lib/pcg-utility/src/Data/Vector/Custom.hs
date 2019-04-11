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
  , zipWithFold
  ) where

import qualified Control.Foldl as L
import qualified Data.Vector   as V
import Data.Vector (Vector)
import Data.Foldable

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

-- |
-- /O(min(n,m))/
--
-- Perform a simulatanous zipWith and fold where the zip function takes values
-- in some monoid.
zipWithFold :: forall a b m . Monoid m => (a -> b -> m) -> Vector a -> Vector b -> m
zipWithFold f va vb =
  let
    la = V.toList va
    lb = V.toList vb
  in
    fold (zipWith f la lb)

    
