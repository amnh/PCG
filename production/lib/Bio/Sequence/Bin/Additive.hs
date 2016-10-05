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
  ( NonAdditiveBin(..)
  ) where


import Bio.Character.Static
import Bio.Sequence.SharedContinugousMetatdata
import Data.List.NonEmpty hiding (length)
import Data.Semigroup
import Data.Monoid               (mappend)
import Data.MonoTraversable      (olength)

data AdditiveBin s
   = AdditiveBin
   { characterStream :: s
   , metatdataBounds :: SharedMetatdataIntervals
   } deriving (Eq,Show)


instance Semigroup s => Semigroup (AdditiveBin s) where

  lhs <> rhs =
    AdditiveBin
      { characterStream = characterStream lhs    <>     characterStream rhs
      , metatdataBounds = metatdataBounds lhs `mappend` metatdataBounds rhs
      }


additiveBin :: NonEmpty (NonEmpty String) -> GeneralCharacterMetadata -> AdditiveBin s
additiveBin staticCharacters corespondingMetadata =
  AdditiveBin
    { characterStream = newChars
    , metatdataBounds = singleton (length staticCharacters) corespondingMetadata
    }
  where
    newChars = encodeStream (characterAlphabet corespondingMetadata) staticCharacters

