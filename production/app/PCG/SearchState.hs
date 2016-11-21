-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.SearchState
-- Copyright   :  () 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

module PCG.SearchState where

import           Bio.Character
import           Bio.Sequence
import           Bio.Sequence.Block
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Evaluation

-- import Debug.Trace

type SearchState = EvaluationT IO (Either TopologicalResult CharacterResult)

type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))

type CharacterResult   = PhylogeneticSolution CharacterDAG

type CharacterDAG      = PhylogeneticDAG
                             (Maybe Double)
                             (Maybe String)
                             StaticCharacterBlock
                             StaticCharacterBlock
                             Double
                             StaticCharacterBlock
                             StaticCharacterBlock
                             DynamicChar

type  UnifiedCharacterSequence
    = CharacterSequence
        StaticCharacterBlock
        StaticCharacterBlock
        Double
        StaticCharacterBlock
        StaticCharacterBlock
        DynamicChar

type  UnifiedCharacterBlock
    = CharacterBlock
        StaticCharacterBlock
        StaticCharacterBlock
        Double
        StaticCharacterBlock
        StaticCharacterBlock
        DynamicChar


-- PhylogeneticDAG (Maybe Double) (Maybe String) (Maybe StaticCharacterBlock) (Maybe DynamicChar)


data  PhylogeneticDAG e n m i c f a d
    = PDAG (ReferenceDAG e (PhylogeneticNode n (CharacterSequence m i c f a d)))
