-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Internal types for implied alignment
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Analysis.ImpliedAlignment.Internal where

import Bio.Character.Encodable
import Bio.Metadata
--import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
--import Bio.PhyloGraph.Tree
import Bio.PhyloGraph.Tree.Binary
import Bio.PhyloGraph.Tree.Referential
import Data.Bits
--import Data.Function.Memoize
import Data.IntMap
import Data.MonoTraversable (Element)
import Data.Vector

-- | (✔)
type SolutionConstraint r m f t n e s = (GeneralSolution r f, MetadataSolution r m, Metadata m s, ForestConstraint f t n e s, Show r)

-- | (✔)
type ForestConstraint       f t n e s = (GeneralForest f t, Show f, TreeConstraint t n e s)

-- | (✔)
type TreeConstraint           t n e s = (BinaryTree t n, Network t n, ReferentialTree t n, NodeConstraint n s, Show t)

-- | (✔)
type NodeConstraint             n   s = (FinalNode n s, IANode n, SeqConstraint s, Show n, EncodedNode n s, PreliminaryNode n s)

-- | (✔)
type SeqConstraint                  s = (EncodableDynamicCharacter s, Bits s, Show s, {- Memoizable s, -} Show (Element s))


-- | The counter tracks the max sequence length and the number of gaps added.
type Counter = (Int, Int)
-- | The counts are a vector of Ints
type Counts = Vector Counter
-- | An alignment object is an 'IntMap' from the node code to a vector of aligned coded sequences.
type Alignment s = IntMap (Vector s)
-- | An alignment over a forest is then a list of 'Alignment's.
type AlignmentForest s = [Alignment s]
-- | And an alignment over a solution is a list of 'AlignmentForest's.
type AlignmentSolution s = [AlignmentForest s]
