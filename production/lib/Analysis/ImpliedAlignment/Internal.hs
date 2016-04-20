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
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Tree
import Bio.Sequence.Coded

import Data.Bits

type SolutionConstraint r m f t n s = (GeneralSolution r f, MetadataSolution r m, Metadata m s, ForestConstraint f t n s)
type ForestConstraint       f t n s = (GeneralForest f t, TreeConstraint t n s)
type TreeConstraint           t n s = (BinaryTree t n, NodeConstraint n s)
type NodeConstraint             n s = (FinalNode n s, SeqConstraint s)
type SeqConstraint                s = (CodedSequence s, Bits s)