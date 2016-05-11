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

import Data.Alphabet
import Data.Foldable                       ()
import Data.Matrix.NotStupid               (Matrix)
import Data.Monoid
import Data.Vector                         (Vector)
import Test.QuickCheck.Arbitrary.Instances ()
import Test.Tasty.QuickCheck

-- TODO: Make 'name' a record type with 2 string fields, fileName and
--       characterName, to avoid ambiguity when retreiving the file in which
--       data was defined.
-- | A container type for various character metadata information.
data CharacterMetadata s
   = CharMeta
   { -- | Stores the type of character
     charType   :: CharDataType
     -- | Alphabet as a list of strings
   , alphabet   :: Alphabet String
     -- | Name (give name : file name)
   , name       :: String
     -- | Whether this character is aligned
   , isAligned  :: Bool
     -- | Whether this character is ignored
   , isIgnored  :: Bool
     -- | The weight of this character, should default to 1
   , weight     :: Double
     -- | The names of the states of this character (corresponds to the alphabet elements)
   , stateNames :: Vector String
     -- | Masks for fitch, should be mempty for anything but NonAdditive
   , fitchMasks :: (s, s)
     -- | Cost of the root for this character
   , rootCost   :: Double
   -- | The cost structure storing different options for costs
   , costs      :: CostStructure
   } deriving (Eq, Show)

-- | Different types of characters are stored here
-- TODO: Add AffineDO, 3dDO, OptimizedDO
data CharDataType = DirectOptimization | Fitch | InfoTheoretic | Unknown deriving (Eq, Show)
--data CharDataType = Nucleotide | AminoAcid | Continuous | Custom | Additive | NonAdditive | Unknown deriving (Eq, Show)

-- | A cost structure can either be a TCM, an affine cost group, or a general cost group
-- AffineCost stores a gap opening, gap continuing, and substitution cost
-- GeneralCost just stores an indelCost and a subCost
data CostStructure = TCM CostMatrix
                      | AffineCost  { gapOpenCost :: Double, gapContinueCost :: Double, subCost :: Double }
                      | GeneralCost { indelCost   :: Double, subCost :: Double } deriving (Eq, Show)

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- TODO: replace these calls with lenses
-- | Prepends a 'String' to the existing character name.
prependName :: String -> CharacterMetadata s -> CharacterMetadata s
prependName n x = x { name     = n <> ":" <> name x }

-- TODO: replace these calls with lenses
-- | Overwrites the existing character alpahbet.
updateAlphabet :: Alphabet String -> CharacterMetadata s -> CharacterMetadata s
updateAlphabet a x = x { alphabet = a }

-- TODO: replace these calls with lenses
-- | Overwrites the existing TCM.
updateTcm :: CostMatrix -> CharacterMetadata s -> CharacterMetadata s
updateTcm      t x = x { costs      = TCM t }

-- TODO: replace these calls with lenses
-- | Overwrites the existing alignment value and optimization value.
updateAligned :: Bool -> CharacterMetadata s -> CharacterMetadata s
updateAligned a x = x { isAligned = a, charType = Fitch }

instance Arbitrary s => Arbitrary (CharacterMetadata s) where
  arbitrary = do
    t <- elements [DirectOptimization, Fitch, InfoTheoretic, Unknown]
    a <- arbitrary
    n <- arbitrary :: Gen String
    align <- arbitrary :: Gen Bool
    ignore <- arbitrary :: Gen Bool
    w <- arbitrary :: Gen Double
    sn <- arbitrary
    fm <- vectorOf 2 arbitrary
    let masks = (head fm, fm !! 1)
    r <- arbitrary :: Gen Double
    randCosts <- vectorOf 3 arbitrary 
    c <- elements [ TCM $ tcmOfSize (length a)
                  , AffineCost  (head randCosts) (randCosts !! 1) (randCosts !! 2)
                  , GeneralCost (head randCosts) (randCosts !! 1)
                  ]
    pure $ CharMeta t a n align ignore w sn masks r c

tcmOfSize :: Int -> CostMatrix
tcmOfSize = undefined
