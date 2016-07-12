-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.ImpliedAlign
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for nodes that work with the implied alignment
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.ImpliedAlign where

import Data.Vector

-- | Represents the Homologies for each character in the sequence.
--   There will be empty 'Homologies' values at indices corresponding to static
--   characters and non-empty 'Homologies' values at indices corresponding to
--   dynamic characters.
type HomologyTrace = Vector Homologies

-- | A 'Vector' of length equal to the length of the Dynamic character to which
--   it corresponds. Index values in the 'Homologies' represent to which new
--   index the corresponding static character should be projected into the larger
--   alignned sequence space. The value at a given index in a 'Homologies' will
--   always be strictly greater than or equal to the itself.
type Homologies    = Vector Int

-- TODO: Possibly remove this type-class.
-- | A node on which 'HomologyTrace' values can be accessed and overwritten.
class IANode n where
    getHomologies  :: n -> HomologyTrace
    setHomologies  :: n -> HomologyTrace -> n

-- | A node on which implied alignments can accessed and overwritten.
class IANode' n s | n -> s where
    getHomologies' :: n -> Vector s
    setHomologies' :: n -> Vector s -> n 
