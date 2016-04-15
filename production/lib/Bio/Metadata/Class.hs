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

-- | Represents a type from which character information can be queried.
class Metadata m s | m -> s where
    getWeight     :: m -> Double
    getIgnored    :: m -> Bool
    getAlphabet   :: m -> Alphabet
    getTcm        :: m -> CostMatrix
    getFitchMasks :: m -> (s, s)
    getAligned    :: m -> Bool
    getType       :: m -> CharDataType
    getIndelCost  :: m -> Double
    getSubCost    :: m -> Double

instance Monoid s => Metadata (CharacterMetadata s) s where
    getWeight     = weight
    getIgnored    = isIgnored
    getAlphabet   = alphabet
    getTcm        = tcm
    getAligned    = isAligned
    getFitchMasks = fitchMasks
    getType       = charType
    getIndelCost  = indelCost
    getSubCost    = subCost
