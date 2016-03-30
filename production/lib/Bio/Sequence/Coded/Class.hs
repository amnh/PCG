-----------------------------------------------------------------------------
--
-- Module      :  Bio.Sequence.Coded.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

import Data.BitVector

-- TODO: require instance of foldable to remove some of these
-- TODO: clear up naming confusion.  

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class Monoid s => CodedSequence s where
    charToSeq   :: BitVector -> s  -- encodes a singleton character as a sequence?
    decode      :: s -> ParsedSeq
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Ord a) => f (t a) -> s
    encode      :: ParsedSeq -> s
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Foldable c, Ord a) => f (t a) -> c a-> s
    encodeOverAlphabet :: ParsedSeq -> Alphabet -> s
    emptySeq    :: s
    filterSeq   :: s -> (BitVector -> Bool) -> s
    grabSubChar :: s -> Int -> Int -> Maybe BitVector
    isEmpty     :: s -> Bool
    numChars    :: s -> Int