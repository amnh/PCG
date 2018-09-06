{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
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
module Data.UnionSet where

import Control.DeepSeq
import Data.Bits
import Data.BitVector.LittleEndian
import GHC.Generics


newtype UnionSet = Union BitVector
  deriving (Bits, Eq, Generic, Ord)

instance Show UnionSet where

    show (Union bv) = foldMap f $ toBits bv
      where
        f x = if x then "1" else "0"

instance NFData UnionSet

instance Semigroup UnionSet where
  (<>) = (.|.)

instance Monoid UnionSet where
  mempty = zeroBits

singletonSet
  :: Int  -- ^ Set size
  -> Int  -- ^ Set index
  -> UnionSet
singletonSet n i = Union . (`setBit` i) $ toEnum n `fromNumber` (0 :: Integer)
