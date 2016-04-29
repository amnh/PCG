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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.Solution.Internal where

import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Solution.Class
import qualified Bio.PhyloGraph.Solution.Metadata as MS
import Bio.Sequence.Parsed
import Bio.Sequence.Coded
import Bio.Metadata.Internal
import Control.Evaluation
import Data.HashMap.Strict
import Data.Monoid
import Data.Vector

-- | The equatable identifier for a node in the graph.
type Identifier = String

-- | The sequence of characters associated with a taxon.
type Sequences = ParsedSequences

-- We'll have two types of node: topological and referential

-- | The character metadata reference structure.
type StandardMetadata = CharacterMetadata DynamicChar

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

-- | Make it an instance of data storage type classes

instance GeneralSolution (Solution d) (Forest d) where
    getForests = forests
    setForests s f = s {forests = f} 

instance MS.MetadataSolution (Solution d) StandardMetadata where
    getMetadata = metadata
    setMetadata solution meta = solution {metadata = meta}

instance Monoid (Solution d) where
    mempty = Solution mempty mempty mempty
    mappend (Solution chars1 meta1 forests1) (Solution chars2 meta2 forests2) = 
        Solution (chars1 <> chars2) (meta1 <> meta2) (forests1 <> forests2)
