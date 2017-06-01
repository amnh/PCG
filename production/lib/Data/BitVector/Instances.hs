-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.Instances
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Defines a 'FiniteBits' instance for 'BitVector'. 
--
-----------------------------------------------------------------------------

{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BitVector.Instances () where

import Data.Bits
import Data.BitVector
import Data.Primitive.ByteArray
import Data.Word
import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms

instance FiniteBits BV where

    {-# INLINE finiteBitSize #-}
    finiteBitSize = size

    {-# INLINE countTrailingZeros #-}
    countTrailingZeros bv = n - lastSetBit - 1
      where
        (n,i) = deconstruct bv
        lastSetBit = I# (integerLog2# i)

    {-# INLINE countLeadingZeros #-}
    countLeadingZeros bv =

        case nat bv of
          S#       v  -> countTrailingZeros $ I# v
          Jp# (BN# v) -> f $ ByteArray v
          Jn# (BN# v) -> f $ ByteArray v

      where
        
        f :: ByteArray -> Int
        f byteArr = g 0 limit
          where
            g :: Int -> Int -> Int
            g i 1 = countTrailingZeros (byteArr `indexByteArray` i :: Word64)
            g i o =
              case countTrailingZeros value of
                64 -> g (i+1) (o-1)
                v  -> v
              where
                value :: Word64
                value = byteArr `indexByteArray` i

            (q, r) = sizeofByteArray byteArr `quotRem` 8
            limit  =
                case r of
                  0 -> q
                  v -> q + v


deconstruct :: BitVector -> (Int,Integer)
deconstruct = (,) <$> size <*> nat
