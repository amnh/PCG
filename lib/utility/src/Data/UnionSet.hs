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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.UnionSet
  ( UnionSet
  , singletonSet
  )
  where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import GHC.Generics
import TextShow                    (TextShow)


-- |
-- Represents the union of several elements with a lexical ordering over a
-- pre-determined finite range. Represented internally as a bit-vector for efficiency.
newtype UnionSet = Union BitVector
  deriving stock    (Generic)
  deriving anyclass (NFData)
  deriving newtype  (Bits, Ord, TextShow)


instance Eq UnionSet where
  {-# SPECIALISE instance Eq UnionSet #-}
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


instance Semigroup UnionSet where
  {-# SPECIALISE instance Semigroup UnionSet #-}
  {-# INLINE (<>) #-}
  (<>) = (.|.)

instance Monoid UnionSet where
  {-# SPECIALISE instance Monoid UnionSet #-}
  {-# INLINE mempty  #-}
  mempty = zeroBits


-- |
-- Create a 'UnionSet with a single element included.
singletonSet
  :: Int  -- ^ Set size
  -> Int  -- ^ Set index
  -> UnionSet
{-# INLINE singletonSet #-}
singletonSet n i = Union . (`setBit` i) $ toEnum n `fromNumber` (0 :: Integer)
