------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Bin.NonAdditive
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Metadata.Sequence.Bin.NonAdditive
  ( NonAdditiveBin(..)
  ) where


import Bio.Metadata.Sequence.SharedContinugousMetatdata


data NonAdditiveBin s
   { characterStream :: s
   , metatdataBounds :: SharedContinugousMetatdata
   } deriving (Eq,Show)

-- TODO: Maybe just a Semigroup?
instance Monoid s => Monoid (NonAdditiveBin s) where

  mempty = NonAdditiveBin mempty mempty

  lhs `mappend` rhs =
    NonAdditiveBin
    { characterStream = characterStream lhs <> characterStream rhs
    , metatdataBounds = metatdataBounds lhs <> metatdataBounds rhs
    }

{-
nonAdditiveBin :: [[String]] -> GeneralCharacterMetadata -> NonAdditiveBin s
nonAdditiveBin staticCharacters corespondingMetadata =
    { characterStream = newChars
    , metatdataBounds = singleton (olength newChars) corespondingMetadata
    }
  where
    newChars = encodeStaticCharacters (alphabet corespondingMetadata) staticCharacters
-}
