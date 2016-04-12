-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution.Data
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
import Bio.Sequence.Parsed
import Bio.Sequence.Coded
import Bio.Metadata.Internal
import Data.HashMap.Strict
import Data.Vector

-- | A forest is a list of dag structures where dags can be referential or topological
type Forest d = [d]

type Identifier = String

type Sequences = ParsedSequences

-- | We'll have two types of node: topological and referential

type StandardMetadata = CharacterMetadata EncodedSeq

type StandardSolution = Solution DAG

-- | A solution is an array of forests
-- character data and names are common across all forests and so stored at this level
data Solution d 
   = Solution
   { parsedChars :: HashMap Identifier Sequences
   , metadata   :: Vector StandardMetadata
   , forests    :: [Forest d]
   } deriving (Eq, Show)


