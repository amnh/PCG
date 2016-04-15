-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Solution representation
--
-----------------------------------------------------------------------------

module Bio.PhyloGraph.Solution.Internal where

import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Forest
import Bio.Sequence.Parsed
import Bio.Sequence.Coded
import Bio.Metadata.Internal
import Control.Evaluation
import Data.HashMap.Strict
import Data.Vector

-- | The equatable identifier for a node in the graph.
type Identifier = String

-- | The sequence of characters associated with a taxon.
type Sequences = ParsedSequences

-- We'll have two types of node: topological and referential

-- | The character metadata reference structure.
type StandardMetadata = CharacterMetadata EncodedSeq

-- | A simple storable computation state value.
type StandardSolution = Solution DAG

-- | A computational evaluation state which can be modified monoidally or
--   monadically.
type SearchState = EvaluationT IO StandardSolution

-- | A solution is an array of forests character data and names are common
--   across all forests and so stored at this level
data Solution d 
   = Solution
   { parsedChars :: HashMap Identifier Sequences
   , metadata   :: Vector StandardMetadata
   , forests    :: [Forest d]
   } deriving (Eq, Show)


