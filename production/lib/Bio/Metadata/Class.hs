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

import qualified Bio.Metadata.Internal as CM
import           Bio.Sequence.Parsed

class Metadata m s | m -> s where
    getWeight :: m -> Double
    getIgnored :: m -> Bool
    getAlphabet :: m -> Alphabet
    getTcm :: m -> CM.CostMatrix
    getFitchMasks :: m -> (s, s)
    getAligned :: m -> Bool

instance Monoid s => Metadata (CM.CharacterMetadata s) s where
    getWeight =        CM.weight
    getIgnored =       CM.isIgnored
    getAlphabet =      CM.alphabet
    getTcm =           CM.tcm
    getAligned =       CM.isAligned
    getFitchMasks =    CM.fitchMasks
