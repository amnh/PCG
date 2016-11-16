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
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Evaluation

-- import Debug.Trace

type SearchState = EvaluationT IO (Either TopologicalResult CharacterResult)

type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))


type CharacterResult   = PhylogeneticSolution
                           (ReferenceDAG
                             (Maybe Double)
                             (PhylogeneticNode
                               (Maybe
                                 (CharacterSequence StaticCharacterBlock DynamicChar))
                             )
                           )

