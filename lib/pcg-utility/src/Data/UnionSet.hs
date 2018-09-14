{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.UnionSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A newtype wrapper for the union monoid instance on sets labelled by
-- the set {1,...,n}.
-----------------------------------------------------------------------------
module Data.UnionSet
  (UnionSet, singletonSet
  )
  where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import GHC.Generics


newtype UnionSet = Union BitVector
  deriving (Bits, Generic, Ord)

{-# SPECIALISE (==) :: UnionSet -> UnionSet -> Bool #-}
instance Eq UnionSet where
  {-# INLINE (==)  #-}
  (==) (Union bv1) (Union bv2) =
    case compare bv1 bv2 of
      EQ -> True
      LT -> (== bv2) $ bv1 .&. bv2
      GT -> (== bv1) $ bv1 .&. bv2


instance Show UnionSet where

    show (Union bv) = foldMap f $ toBits bv
      where
        f x = if x then "1" else "0"

instance NFData UnionSet

{-# SPECIALISE (<>) :: UnionSet -> UnionSet -> UnionSet #-}
instance Semigroup UnionSet where
  {-# INLINE (<>) #-}
  (<>) = (.|.)

{-# SPECIALISE mempty :: UnionSet #-}
instance Monoid UnionSet where
  {-# INLINE mempty  #-}
  mempty = zeroBits


singletonSet
  :: Int  -- ^ Set size
  -> Int  -- ^ Set index
  -> UnionSet
{-# INLINE singletonSet #-}
singletonSet n i = Union . (`setBit` i) $ toEnum n `fromNumber` (0 :: Integer)
