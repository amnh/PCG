------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Bin.Additive
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Sequence.Bin.Additive
  ( AdditiveBin(..)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.List.NonEmpty hiding (length)
import Data.Semigroup
import Data.Monoid               (mappend)
import Data.MonoTraversable      (olength)

data AdditiveBin s
   = AdditiveBin
   { characterDecoration :: s
   , metatdataBounds     :: SharedMetatdataIntervals
   } deriving (Eq,Show)


instance Semigroup s => Semigroup (AdditiveBin s) where

  lhs <> rhs =
    AdditiveBin
      { characterDecoration = characterDecoration lhs    <>     characterDecoration rhs
      , metatdataBounds     = metatdataBounds     lhs `mappend` metatdataBounds     rhs
      }
