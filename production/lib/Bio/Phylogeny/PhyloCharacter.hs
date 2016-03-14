-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
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

import Data.Vector (Vector)
import GHC.Generics
import Data.Matrix.NotStupid (Matrix, fromList, (<|>))



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
data PhyloCharacter b = DNA         { name :: String -- The character name if it has one
                                    , aligned :: Bool -- Whether the character represents an aligned or unaligned, sequence
                                    , fitchMasks :: (Vector b, Vector b)
                                    , stateNames :: Vector String
                                    , alphabet :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    }
 
                      | RNA         { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (Vector b, Vector b)
                                    , stateNames :: Vector String
                                    , alphabet :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    }

                      | Qualitative { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (Vector b, Vector b)
                                    , stateNames :: Vector String
                                    , alphabet :: Vector String
                                    , tcm :: CostMatrix
                                    , additive :: Bool
                                    , ignored :: Bool
                                    } 

                      | Continous   { name :: String
                                    , ignored :: Bool
                                    , tcm :: CostMatrix
                                    }
 
                      | Custom      { name :: String
                                    , aligned :: Bool
                                    , masks :: (Vector b, Vector b)
                                    , alphabet :: Vector String
                                    , stateNames :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    , additive :: Bool
                                    } 
 
                      | AminoAcid   { name :: String
                                    , aligned :: Bool
                                    , fitchMasks :: (Vector b, Vector b)
                                    , alphabet :: Vector String
                                    , stateNames :: Vector String
                                    , tcm :: CostMatrix
                                    , ignored :: Bool
                                    }

                       deriving (Show, Eq, Generic)

-- | A cost matrix is just a matrix of floats
type CostMatrix = Matrix Double

-- For convenience have a monoid instance even though mappend is somewhat arbitrary
instance Monoid CostMatrix where
  mempty = fromList 0 0 []
  mappend = (<|>)
