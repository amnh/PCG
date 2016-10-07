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
  ( AdditiveBin(characterDecoration, metatdataBounds)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.Semigroup
import Data.Monoid               (mappend)

data AdditiveBin s
   = AdditiveBin
   { characterDecoration :: s
   , metatdataBounds     :: SharedMetatdataIntervals
   , width               :: !Int
   } deriving (Eq,Show)


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
