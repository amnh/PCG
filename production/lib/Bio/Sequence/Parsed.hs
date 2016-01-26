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

module Bio.Sequence.Parsed (ParsedSeq, TreeSeqs) where

import Data.Vector
import Data.Map.Lazy

type ParsedSeq = Vector [String]

type TreeSeqs = Map String ParsedSeq