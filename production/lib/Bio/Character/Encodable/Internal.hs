-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Encodable.Internal where


-- |
-- Represents a type which stores one or more abiuguity groups from an alphabet.
-- Allows /O(1)/ derivation of the number of possiblly prsent symbols in the
-- ambiguity group.
class EncodedAmbiguityGroupContainer w where

    symbolCount :: w -> Int


class PossiblyMissingCharacter c where

    toMissing :: c -> c

    isMissing :: c -> Bool


instance PossiblyMissingCharacter c => PossiblyMissingCharacter (Maybe c) where

    toMissing = fmap toMissing

    isMissing = maybe False isMissing
