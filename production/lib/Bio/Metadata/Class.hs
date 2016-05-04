-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Metadata.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for a set of metadata
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Metadata.Class where

import Bio.Metadata.Internal 
import Bio.Sequence.Parsed
import Data.Matrix (getElem)

-- | Represents a type from which character information can be queried.
class Metadata m s | m -> s where
    getWeight         :: m -> Double
    getIgnored        :: m -> Bool
    getAlphabet       :: m -> Alphabet
    getCosts          :: m -> CostStructure
    getFitchMasks     :: m -> (s, s)
    getAligned        :: m -> Bool
    getType           :: m -> CharDataType
    getGapCost        :: m -> Double

instance Metadata (CharacterMetadata s) s where
    getWeight         = weight
    getIgnored        = isIgnored
    getAlphabet       = alphabet
    getCosts          = costs
    getAligned        = isAligned
    getFitchMasks     = fitchMasks
    getType           = charType
    getGapCost      m = case costs m of
                            TCM mat           -> if (length $ alphabet m) > 1 then getElem 0 1 mat else 1
                            AffineCost  g _ _ -> g
                            GeneralCost g _   -> g
    
