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
import           Bio.Character.Parsed
import           Bio.Sequence
import           Bio.Sequence.Block
import           Bio.Metadata.CharacterName hiding (sourceFile)
import           Bio.Metadata.Parsed
import           Bio.PhyloGraph.Solution    hiding (parsedChars)
import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Forest.Parsed
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Component
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Arrow                     ((&&&))
import           Control.Applicative               ((<|>))
import           Control.Evaluation
import           Data.Alphabet
import           Data.Bifunctor                    (first)
import           Data.Foldable
import qualified Data.IntSet                as IS
import           Data.Key
import           Data.List                         (transpose, zip4)
import           Data.List.NonEmpty                (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty         as NE  (fromList)
import           Data.List.Utility                 (duplicates)
import           Data.Map                          (Map, intersectionWith, keys)
import qualified Data.Map                   as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.Semigroup                    ((<>), sconcat)
import           Data.Semigroup.Foldable
import           Data.Set                          (Set, (\\))
import qualified Data.Set                   as Set
import           Data.TCM                          (TCM)
import qualified Data.TCM                   as TCM
import           Data.MonoTraversable
import           Data.Vector                       (Vector)
import           PCG.Command.Types.Read.Unification.UnificationError
import           Prelude                    hiding (lookup, zip, zipWith)

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

