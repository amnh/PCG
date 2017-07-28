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

{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Bio.Sequence.Bin.NonAdditive
  ( NonAdditiveBin(..)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContiguousMetadata
import Data.Monoid          (mappend)
import Data.Semigroup


-- |
-- A bin of one or more non-additive characters and thier corresponding metadata.
--
-- Use '(<>)' to construct larger bins with differing metadata.
--
-- There is currently no singleton-like constructor!
data NonAdditiveBin s
   = NonAdditiveBin
   { characterDecoration :: s
   , metatdataBounds     :: SharedMetatdataIntervals
   } deriving (Eq,Functor,Show)


instance EncodedAmbiguityGroupContainer s => EncodedAmbiguityGroupContainer (NonAdditiveBin s) where

    {-# INLINE symbolCount #-}
    symbolCount = symbolCount . characterDecoration


instance Semigroup s => Semigroup (NonAdditiveBin s) where

  lhs <> rhs =
    NonAdditiveBin
      { characterDecoration = characterDecoration lhs    <>     characterDecoration rhs
      , metatdataBounds     = metatdataBounds     lhs `mappend` metatdataBounds     rhs
      }
