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
    { charType   :: CharDataType
    , alphabet   :: Alphabet
    , name       :: String
    , isAligned  :: Bool
    , isAdditive :: Bool
    , isIgnored  :: Bool
    , weight     :: Double
    , tcm        :: CostMatrix
    , stateNames :: Vector String
    , fitchMasks :: (s, s)
    } deriving (Eq, Show)

data CharDataType = DNA | RNA | AminoAcid | Continuous | Custom | Qualitative | Unknown deriving (Eq, Show)

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- For convenience have a monoid instance even though mappend is somewhat arbitrary
instance Monoid CostMatrix where
  mempty = fromList 0 0 []
  mappend = (<|>)

prependName :: String -> CharacterMetadata s -> CharacterMetadata s
prependName    n x = x { name     = n <> ":" <> name x }

updateAlphabet :: Alphabet -> CharacterMetadata s -> CharacterMetadata s
updateAlphabet a x = x { alphabet = a }

updateTcm :: CostMatrix -> CharacterMetadata s -> CharacterMetadata s
updateTcm      t x = x { tcm      = t }

updateAligned :: Bool -> CharacterMetadata s -> CharacterMetadata s
updateAligned a x = x { isAligned = a }