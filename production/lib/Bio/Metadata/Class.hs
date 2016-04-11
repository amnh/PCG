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

class Metadata m s | m -> s where
    getWeight :: m -> Double
    getIgnored :: m -> Bool
    getAlphabet :: m -> Alphabet
    getTcm :: m -> CostMatrix
    getFitchMasks :: m -> (s, s)
    getAligned :: m -> Bool
    getAdditive :: m -> Bool

instance Monoid s => Metadata (CharacterMetadata s) s where
    getWeight =        weight
    getIgnored =       isIgnored
    getAlphabet =      alphabet
    getTcm =           tcm
    getAligned =       isAligned
    getFitchMasks =    fitchMasks
    getAdditive m =    charType m == Additive
