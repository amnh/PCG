{-# LANGUAGE TypeFamilies #-}
module Data.BitMatrix where

import Data.Bifunctor
import Data.Bits
import Data.BitVector
import Data.Monoid
import Data.MonoTraversable

data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Show)

type instance Element BitMatrix = BitVector

numCols :: BitMatrix -> Int
numCols (BitMatrix n _) = n

numRows :: BitMatrix -> Int
numRows (BitMatrix n bv) = width bv `div` n

rows :: BitMatrix -> [BitVector]
rows bm@(BitMatrix n bv) = (bv @@) <$> slices
  where
    m = numRows bm
    slices = take m $ iterate ((+n) `bimap` (+n)) (n-1, 0)
        
instance MonoFunctor BitMatrix where
  omap f bm = BitMatrix (numCols bm) . mconcat $ f <$> rows bm
