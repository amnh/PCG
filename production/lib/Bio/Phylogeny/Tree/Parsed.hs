-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Phylogeny.Graph
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for a parsed tree with function to go from encoded to parsed
--
-----------------------------------------------------------------------------

module Bio.Phylogeny.Tree.Parsed where

class ParsedTree t o | o -> t where
    encode :: t -> o
    decode :: o -> t