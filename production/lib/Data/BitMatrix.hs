{-# LANGUAGE TypeFamilies #-}
module Data.BitMatrix where

import Data.Bits
import Data.BitVector
import Data.Monoid
import Data.MonoTraversable

data BitMatrix
   = BitMatrix !Int BitVector
   deriving (Show)

type instance Element BitMatrix = BitVector

cols :: BitMatrix -> Int
cols (BitMatrix n _) = n

rows :: BitMatrix -> Int
rows (BitMatrix n bv) = width bv `div` n

