-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Packed.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of packed sequences
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Bio.Sequence.Packed.Class where

import Bio.Sequence.Parsed
import Bio.Sequence.Coded.Class

-- | A coded sequence allows grabbing of a character, filtering, and some standard types
class (Monoid s, CodedSequence s b) => PackedSequence s b | s -> b where
    packOverAlphabet :: ParsedSeq -> [String] -> s