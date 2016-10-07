-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Internal where


-- |
-- Represents a type which stores one or more abiuguity groups from an alphabet.
-- Allows /O(1)/ derivation of the number of possiblly prsent symbols in the
-- ambiguity group.
class EncodedAmbiguityGroupContainer w where

  symbolCount :: w -> Int
