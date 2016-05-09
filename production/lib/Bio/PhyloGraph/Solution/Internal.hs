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

import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Solution.Class
import qualified Bio.PhyloGraph.Solution.Metadata as MS
import           Bio.Character.Parsed
import           Bio.Character.Dynamic.Coded
import           Bio.Metadata.Internal

import           Control.Evaluation
import           Data.HashMap.Strict (HashMap, fromList)
--import           Data.Monoid
import           Data.Vector (Vector, toList)
import qualified Data.Vector as V (map, fromList)
import           Test.Tasty.QuickCheck 

-- | The equatable identifier for a node in the graph.
type Identifier = String

-- TODO: ParsedDynChars should probably not be hard coded here.
-- TODO: Actually, why do we need this at all?
-- | The sequence of characters associated with a taxon.
type Sequences = ParsedChars

-- We'll have two types of node: topological and referential

-- TODO: DynamicChar should probably not be hard coded here.
-- | The character metadata reference structure.
type StandardMetadata = CharacterMetadata DynamicChar

-- | A simple storable computation state value.
type StandardSolution = Solution DAG

-- | A computational evaluation state which can be modified monoidally or
--   monadically.
type SearchState = EvaluationT IO StandardSolution

-- | A structure for storing parsed characters
type Parsed = HashMap Identifier Sequences

-- | A solution is an array of forests character data and names are common
--   across all forests and so stored at this level
data Solution d
   = Solution
   { parsedChars :: Parsed
   , metadata    :: Vector StandardMetadata
   , forests     :: [Forest d]
   } deriving (Eq, Show)

-- | Make it an instance of data storage type classes

instance GeneralSolution (Solution d) (Forest d) where
    getForests     = forests
    setForests s f = s {forests = f}

instance MS.MetadataSolution (Solution d) StandardMetadata where
    getMetadata               = metadata
    setMetadata solution meta = solution {metadata = meta}

instance Arbitrary (Solution DAG) where
    arbitrary = do
      meta <- V.fromList <$> listOf (arbitrary :: Gen StandardMetadata)
      chars <- arbitraryCharsGivenMeta meta
      f <- listOf $ listOf (arbitraryDAGGS chars meta)
      pure $ Solution chars meta f

arbitraryCharsGivenMeta :: Vector StandardMetadata -> Gen Parsed
arbitraryCharsGivenMeta allMeta = do
  names <- listOf (arbitrary :: Gen String)
  let numNodes = length names
  let alphs = toList $ V.map alphabet allMeta
  parsed <- vectorOf numNodes (parsedCharsGivenAlph alphs)
  pure $ fromList $ zip names parsed