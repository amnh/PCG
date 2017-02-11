 ------------------------------------------------------------------------------
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
import Data.Matrix.NotStupid               (Matrix, getElem, matrix)
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
     charType   :: CharDataType                      -- TODO: Kill this, but each type will be rebinned into the types below
                                                     --    (four for static, three for dynamic)
                                                     -- static:  additive (ordered), non-additive (Fitch, unordered), Sankoff, continuous
                                                     -- dynamic: sequence (IDS), rearrangement (IDR, no S), sequence with move (IDSR)
                                                     --    where I is insertion, D is deletion, S is substitution R is rearrangement
     -- | Alphabet as a list of strings
   , alphabet   :: Alphabet String                   -- This will become Maybe BiMap with alphabet symbols & state names
                                                     --    state names correspond to the alphabet elements; if not given, alphabet values are default
                                                     -- Possibly combined with TCM.
     -- | Name (give name : file name)
   , name       :: String                            -- TODO: change to
                                                     -- tuple with (Name,Int)              (where Int is global index)
                                                     -- Name
                                                     --    | UserDefined :: String          (String value is username)
                                                     --    | Default     :: (FilePath,Int)  (String value is filename, Int value is index within file)
     -- | Whether this character is aligned
   , isAligned  :: Bool                              -- TODO: Kill this
     -- | Whether this character is ignored
   , isIgnored  :: Bool                              -- this is mutable; things will be rebinned
     -- | The weight of this character,
     -- should default to 1
   , weight     :: Double
     -- | The names of the states of this character
   , stateNames :: Vector String                     -- TODO: kill this; it's being added to alphabet
     -- | Masks for fitch, should be mempty for
     -- anything but NonAdditive
   , fitchMasks :: (s, s)                            -- TODO: only static non-additive
     -- | Cost of the root for this character
   , rootCost   :: Double                            -- additive factor for both static and dynamic chars (maybe rename)
   -- | The cost structure storing different
   -- options for costs
   , costs      :: CostStructure

   -- TODO: -- AffineCost :: Maybe Int

   } deriving (Eq)


-- | (✔)
instance Show (CharacterMetadata s) where
  show s = unlines
     [ "Metadata: "
     , "{ Alphabet:       " <> show (alphabet   s)
     , ", Name:           " <> show (name       s)
     , ", Is Aligned:     " <> show (isAligned  s)
     , ", Is Ignored:     " <> show (isIgnored  s)
     , ", Weight:         " <> show (weight     s)
     , ", State Names:    " <> show (stateNames s)
     , ", Root Cost:      " <> show (rootCost   s)
     , ", Cost Structure: "
     , unlines . fmap ("    " <>) . lines . show $ costs s
     , "}"
     ]

-- TODO: Kill this, too
-- | Different types of characters are stored here
-- TODO: Add AffineDO, 3dDO, OptimizedDO
data CharDataType = DirectOptimization | Fitch | InfoTheoretic | Unknown deriving (Eq, Show) -- replace this with above
--data CharDataType = Nucleotide | AminoAcid | Continuous | Custom | Additive | NonAdditive | Unknown deriving (Eq, Show)

-- | A cost structure can either be a TCM, an affine cost group, or a general cost group
-- AffineCost stores a gap opening, gap continuing, and substitution cost TODO: affine might have a CostMatrix, as well.
-- GeneralCost just stores an indelCost and a subCost
data CostStructure = TCM CostMatrix
                   | AffineCost  { gapCost :: Int, gapExtendCost :: Int, substitutionCost :: Int }
                   | GeneralCost { indelCost :: Double, subCost :: Double } deriving (Eq, Show) -- TODO: should be Ints, not Doubles



-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Int

toCostFunction :: CostStructure -> Word -> Word -> Word
toCostFunction (TCM tcm) = \i j -> toEnum $ getElem (fromEnum i) (fromEnum j) tcm
toCostFunction        _  = \_ _ -> 1



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

-- TODO: Possibly inconsistent generation, need to review dependant structures
-- | Contains a TCM of equally costly character transitions if not an aligned character.
instance Arbitrary s => Arbitrary (CharacterMetadata s) where
  arbitrary = do
    t         <- elements [DirectOptimization, Fitch, InfoTheoretic, Unknown]
    a         <- arbitrary
    n         <- arbitrary :: Gen String
    align     <- arbitrary :: Gen Bool
    ignore    <- arbitrary :: Gen Bool
    w         <- arbitrary :: Gen Double
    sn        <- arbitrary
    fm        <- vectorOf 2 arbitrary
    let masks = (head fm, fm !! 1)
    r         <- arbitrary :: Gen Double
    randCosts <- vectorOf 3 arbitrary
    randIntCosts <- vectorOf 3 arbitrary
    c         <- elements
                 [ TCM $ tcmOfSize (length a)
                 , AffineCost  (head randIntCosts) (randIntCosts !! 1) (randIntCosts !! 2)
                 , GeneralCost (head randCosts) (randCosts !! 1)
                 ]
    pure $ CharMeta t a n align ignore w sn masks r c

-- | A default TCM matrix of equal cost character transitions.
tcmOfSize :: Int -> CostMatrix
tcmOfSize n = matrix n n (const 1)
