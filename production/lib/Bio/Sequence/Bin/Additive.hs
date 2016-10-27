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

{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Bio.Sequence.Bin.Additive
  ( AdditiveBin(characterDecoration, metatdataBounds)
  ) where

import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.Semigroup
import Data.Monoid               (mappend)


-- |
-- A bin of one or more additive characters and thier corresponding metadata.
--
-- Use '(<>)' to construct larger bins with differing metadata.
--
-- There is currently no singleton-like constructor!
data AdditiveBin s
   = AdditiveBin
   { characterDecoration :: s
   , metatdataBounds     :: SharedMetatdataIntervals
   , width               :: !Int
   } deriving (Eq, Functor, Show)


instance EncodedAmbiguityGroupContainer (AdditiveBin s) where

    {-# INLINE symbolCount #-}
    symbolCount = width
    

instance Semigroup s => Semigroup (AdditiveBin s) where

    lhs <> rhs =
      AdditiveBin
        { characterDecoration = characterDecoration lhs    <>     characterDecoration rhs
        , metatdataBounds     = metatdataBounds     lhs `mappend` metatdataBounds     rhs
        , width               = width               lhs
        }
