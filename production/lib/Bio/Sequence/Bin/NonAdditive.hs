------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Bin.NonAdditive
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Sequence.Bin.NonAdditive
  ( NonAdditiveBin(..)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.List.NonEmpty
import Data.Monoid          (mappend)
import Data.MonoTraversable (olength)
import Data.Semigroup


data NonAdditiveBin s
   = NonAdditiveBin
   { characterDecoration :: s
   , metatdataBounds     :: SharedMetatdataIntervals
   } deriving (Eq,Show)


instance Semigroup s => Semigroup (NonAdditiveBin s) where

  lhs <> rhs =
    NonAdditiveBin
      { characterDecoration = characterDecoration lhs    <>     characterDecoration rhs
      , metatdataBounds     = metatdataBounds     lhs `mappend` metatdataBounds     rhs
      }
