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

-- TODO: Make 'name' a record type with 2 string fields, fileName and
--       characterName, to avoid ambiguity when retreiving the file in which
--       data was defined.
-- | A container type for various character metadata information.
data CharacterMetadata s
   = CharMeta
   { -- | Stores the type of character
     charType   :: CharDataType
     -- | Alphabet as a list of strings
   , alphabet   :: Alphabet      
     -- | Name (give name : file name)
   , name       :: String
     -- | Whether this character is aligned
   , isAligned  :: Bool
     -- | Whether this character is ignored
   , isIgnored  :: Bool
     -- | The weight of this character, should default to 1
   , weight     :: Double
     -- | The tcm expressing transition weights between values for this character
   , tcm        :: CostMatrix
     -- | The names of the states of this character (corresponds to the alphabet elements)
   , stateNames :: Vector String
     -- | Masks for fitch, should be mempty for anything but NonAdditive
   , fitchMasks :: (s, s)
     -- | Cost of the root for this character
   , rootCost   :: Double
   -- | Cost to open a gap (will only be relevant for some optimization types)
   , indelOpenCost :: Double
   -- | Cost to continue a gap, or when affine is not used, the general gapCost
   , indelCost :: Double
   -- | Cost to do a substitution at this character
   , subCost :: Double
   } deriving (Eq, Show)

-- | Different types of characters are stored here
-- TODO: rename to optimization type or heuristic type
-- TODO: Add AffineDO, 3dDO, OptimizedDO
data CharDataType = DirectOptimization | Fitch | InfoTheoretic | Unknown deriving (Eq, Show)
--data CharDataType = Nucleotide | AminoAcid | Continuous | Custom | Additive | NonAdditive | Unknown deriving (Eq, Show)

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- For convenience have a monoid instance even though mappend is somewhat arbitrary
-- TODO: This instance is probably a bad idea, doesn't follow monoid laws (killjoy)
-- | Does not folow 'Monoid' laws. Should be revised or removed.
instance Monoid CostMatrix where
  mempty  = fromList 0 0 []
  mappend = (<|>)

-- TODO: replace these calls with lenses
-- | Prepends a 'String' to the existing character name. 
prependName :: String -> CharacterMetadata s -> CharacterMetadata s
prependName n x = x { name     = n <> ":" <> name x }

-- TODO: replace these calls with lenses
-- | Overwrites the existing character alpahbet. 
updateAlphabet :: Alphabet -> CharacterMetadata s -> CharacterMetadata s
updateAlphabet a x = x { alphabet = a }

-- TODO: replace these calls with lenses
-- | Overwrites the existing TCM.
updateTcm :: CostMatrix -> CharacterMetadata s -> CharacterMetadata s
updateTcm      t x = x { tcm      = t }

-- TODO: replace these calls with lenses
-- | Overwrites the existing alignment value and optimization value.
updateAligned :: Bool -> CharacterMetadata s -> CharacterMetadata s
updateAligned a x = x { isAligned = a, charType = Fitch }
