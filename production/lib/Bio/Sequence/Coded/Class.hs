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

{- This is Sequence/Coded/Class module, which should be renamed. -}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Coded.Class where

--import Bio.Sequence.Character.Coded
import Bio.Sequence.Parsed

import Data.BitVector

-- TODO: require instance of foldable to remove some of these
-- TODO: clear up naming confusion.  

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class Monoid s => CodedSequence s where
    decodeOverAlphabet   :: s -> Alphabet -> ParsedSeq
    decodeOneChar        :: s -> Alphabet -> ParsedSeq 
    -- TODO: This should be translated to:
    -- encode :: (Foldable f, Functor f, Foldable t, Foldable c, Ord a) => f (t a) -> c a -> s
    encodeOverAlphabet   :: ParsedSeq -> Alphabet -> s
    encodeOneChar        :: Alphabet -> AmbiguityGroup -> s
    emptySeq             :: s
    filterGaps           :: s -> s -> Alphabet -> s
    gapChar              :: Int -> s
    grabSubChar          :: s -> Int -> Int -> s
    isEmpty              :: s -> Bool
    numChars             :: s -> Int -> Int