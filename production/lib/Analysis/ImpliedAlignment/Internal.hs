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
{-# LANGUAGE ConstraintKinds #-}

module Analysis.ImpliedAlignment.Internal where

import Bio.Metadata
import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Tree
import Bio.Sequence.Coded

import Data.Bits
import Data.Function.Memoize
import Data.IntMap
import Data.Vector

type SolutionConstraint r m f t n e s = (GeneralSolution r f, MetadataSolution r m, Metadata m s, ForestConstraint f t n e s)
type ForestConstraint       f t n e s = (GeneralForest f t, TreeConstraint t n e s)
type TreeConstraint           t n e s = (BinaryTree t n, Network t n, StandardDAG t n e, NodeConstraint n s)
type NodeConstraint             n   s = (FinalNode n s, IANode n, RefNode n, SeqConstraint s)
type SeqConstraint                  s = (EncodableDynamicCharacter s, Bits s, Show s, Memoizable s, DynamicCoded s)

-- The counts are a vector of ints
type Counts = Vector Int
-- | An alignment object is an intmap from the node code to a vector of aligned coded sequences
type Alignment s = IntMap (Vector s)
-- | An alignment over a forest is then a list of alignments
type AlignmentForest s = [Alignment s]
-- | And an alignment over a solution is a list of forest alignments
type AlignmentSolution s = [AlignmentForest s]