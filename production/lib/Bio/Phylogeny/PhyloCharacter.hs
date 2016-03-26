-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.PhyloCharacter
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structure for a PhyloCharacter character info
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Phylogeny.PhyloCharacter where

--import Bio.Sequence.Coded
import Bio.Sequence.Parsed
import GHC.Generics
import Data.Matrix.NotStupid (Matrix, fromList, (<|>))
import Data.Monoid           ((<>))
import Data.Vector           (Vector)



{-
data CharacterMetadata 
   = CharacterMetadata
   { name      :: String
   , isAligned :: Bool
   , charType  :: CharDataType
   , alphabet  :: AmbiguityGroup
   , ignored   :: Bool
   , costM     :: CostMatrix
   } deriving (Show)

makeDNA :: String -> Bool -> (Vector b, Vector b) -> Vector String -> Vector String -> CostMatrix -> Bool -> CharacterMetadata b
makeDNA name' aligned' fitchMasks' stateNames' alphabet' tcm' ignored' =
    CharacterMetaData name' aligned' DNA fitchMasks' stateNames' alphabet' ignored'  tcm'

makeContinuous :: String -> Bool -> CostMatrix -> Vector String -> CharacterMetaData b
makeContinuous name' ignored' tcm' stateNames' =
    CharacterMetaData name' True Continuous mempty stateNames' mempty ignored'  tcm'
-}

-- | Define a character type as DNA, RNA, Morphology, Continous, or Custom
-- Fields differ based on the constructor, but in general all hold a name, ignored, and tcm
data PhyloCharacter s = DNA         { name :: String -- The character name if it has one
                                    , aligned :: Bool -- Whether the character represents an aligned or unaligned, sequence
                                    , fitchMasks :: (s, s)
                                    , stateNames :: Vector String
                                    , alphabet :: Alphabet
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    , weight :: Double
                                    }
 
                      | RNA         { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (s, s)
                                    , stateNames :: Vector String
                                    , alphabet :: Alphabet
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    , weight :: Double
                                    }

                      | Qualitative { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (s, s)
                                    , stateNames :: Vector String
                                    , alphabet :: Alphabet
                                    , tcm :: CostMatrix
                                    , additive :: Bool
                                    , ignored :: Bool
                                    , weight :: Double
                                    } 
                        -- TODO: fix spelling below
                      | Continous   { name :: String -- TODO: Add step values
                                    , ignored :: Bool
                                    , tcm :: CostMatrix
                                    , alphabet :: Alphabet
                                    , weight :: Double
                                    }
 
                      | Custom      { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (s, s)
                                    , alphabet :: Alphabet
                                    , stateNames :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    , additive :: Bool
                                    , weight :: Double
                                    } 
 
                      | AminoAcid   { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (s, s)
                                    , alphabet :: Alphabet
                                    , stateNames :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    , weight :: Double
                                    }

                       deriving (Show, Eq, Generic)

                        -- TODO: add a structure to track the root position for every character where it was optimized
                        -- TODO: reduce number of constructors
                        -- TODO: think about how this may change over time to track optimization type
                        -- TODO: make sure character names track with file name (parser?)


-- TODO: Replace calls with lenses!
prependName :: String -> PhyloCharacter s -> PhyloCharacter s
prependName    n x = x { name     = n <> ":" <> name x }

updateAlphabet :: Alphabet -> PhyloCharacter s -> PhyloCharacter s
updateAlphabet a x = x { alphabet = a }

updateTcm :: CostMatrix -> PhyloCharacter s -> PhyloCharacter s
updateTcm      t x = x { tcm      = t }

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- For convenience have a monoid instance even though mappend is somewhat arbitrary
instance Monoid CostMatrix where
  mempty = fromList 0 0 []
  mappend = (<|>)
