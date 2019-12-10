-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Normalization.Character.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module holding the data type for a parsed character, which is the type
-- that comes from the parsers, and is then coverted into our various internal
-- character types
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Normalization.Character.Internal where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Data
import           Data.Foldable
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Semigroup.Foldable
import           Data.Text.Short         (ShortText)
import           Data.Vector.NonEmpty    (Vector)
import           File.Format.Fastc       (CharacterSequence)
import           GHC.Generics


-- |
-- A mapping from taxon identifiers to their corresponding sequences.
type NormalizedCharacters = Map Identifier NormalizedCharacterCollection


-- |
-- Represents a character sequence containing possibly-missing character data.
type NormalizedCharacterCollection = Vector NormalizedCharacter


-- |
-- The string value that uniquely identifies a taxon.
type Identifier = ShortText


-- |
-- A generalized character type extracted from a parser.
-- A character can be real-valued, discrete and singular,
-- or discrete with variable length.
data NormalizedCharacter
   = NormalizedContinuousCharacter (Maybe Double)
   | NormalizedDiscreteCharacter   (Maybe (AmbiguityGroup ShortText))
   | NormalizedDynamicCharacter    (Maybe (NonEmpty (AmbiguityGroup ShortText)))
   deriving stock    (Data, Eq, Ord, Generic, Show, Typeable)
   deriving anyclass (NFData)


-- |
-- Take a single, unambiguous base and constructs a dynamic character value.
parsedDynamicCharacterFromShortText :: ShortText -> NormalizedCharacter
parsedDynamicCharacterFromShortText = NormalizedDynamicCharacter . pure . pure . pure


-- |
-- Take a sequence of characters and create a singleton collection of a dynamic character.
convertCharacterSequenceLikeFASTA :: CharacterSequence -> NormalizedCharacterCollection
convertCharacterSequenceLikeFASTA =
    pure . NormalizedDynamicCharacter . Just . toNonEmpty . fmap toNonEmpty


-- |
-- Takes a 'Foldable' structure of 'Map's and returns the union 'Map'
-- containing all the key-value pairs. This fold is right biased with respect
-- to duplicate keys. When identical keys occur in multiple 'Map's, the value
-- occurring last in the 'Foldable' structure is returned.
mergeMaps :: (Foldable t, Ord k) => t (Map k v) -> Map k v
mergeMaps = foldl' (M.unionWith (flip const)) mempty
