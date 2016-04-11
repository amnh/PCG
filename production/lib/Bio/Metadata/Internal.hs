-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data types for metadata
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Metadata.Internal where

import Bio.Sequence.Parsed
import Data.Matrix.NotStupid (Matrix, fromList, (<|>))
import Data.Monoid
import Data.Vector (Vector)

data CharacterMetadata s
    = CharMeta
    { charType   :: CharDataType -- Stores the type of character
    , alphabet   :: Alphabet -- Alphabet as a list of strings
    , name       :: String -- Name (give name : file name) TODO: make this a tuple to avoid ambiguity
    , isAligned  :: Bool -- Whether this character is aligned
    , isIgnored  :: Bool -- Whether this character is ignored
    , weight     :: Double -- The weight of this character, should default to 1
    , tcm        :: CostMatrix -- The tcm expressing transition weights between values for this character
    , stateNames :: Vector String -- The names of the states of this character (corresponds to the alphabet elements)
    , fitchMasks :: (s, s) -- Masks for fitch, should be mempty for anything but NonAdditive
    , rootCost   :: Double -- Cost of the root for this character
    } deriving (Eq, Show)

-- | Different types of characters are stored here
data CharDataType = Nucleotide | AminoAcid | Continuous | Custom | Additive | NonAdditive | Unknown deriving (Eq, Show)

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- For convenience have a monoid instance even though mappend is somewhat arbitrary
-- TODO: This instance is probably a bad idea, doesn't follow monoid laws
instance Monoid CostMatrix where
  mempty  = fromList 0 0 []
  mappend = (<|>)

prependName :: String -> CharacterMetadata s -> CharacterMetadata s
prependName    n x = x { name     = n <> ":" <> name x }

updateAlphabet :: Alphabet -> CharacterMetadata s -> CharacterMetadata s
updateAlphabet a x = x { alphabet = a }

updateTcm :: CostMatrix -> CharacterMetadata s -> CharacterMetadata s
updateTcm      t x = x { tcm      = t }

updateAligned :: Bool -> CharacterMetadata s -> CharacterMetadata s
updateAligned a x = x { isAligned = a }
