-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module holding the data type for a parsed sequence
--
-----------------------------------------------------------------------------

module Bio.Sequence.Parsed (ParsedSeq, TreeSeqs, Alphabet) where

import Data.Vector
import Data.Map.Lazy

type Character = Maybe (Vector [String])

type ParsedSeq = Vector Character

type TreeSeqs = Map String ParsedSeq

type Alphabet = Vector [String]