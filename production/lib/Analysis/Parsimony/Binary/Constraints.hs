-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Constraints
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions used accross optimization modules
--
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts #-}

module Analysis.Parsimony.Binary.Constraints where

import Bio.Character.Dynamic.Coded
import Bio.Metadata
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Node ()
import Bio.PhyloGraph.Node.Encoded
import Bio.PhyloGraph.Node.Final
import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Tree.Referential
import Bio.PhyloGraph.Tree.Binary
import Data.Bits
import Data.Function.Memoize
import Data.MonoTraversable

--TODO: Seriously?
type SolutionConstraint' r f t n s m = (GeneralSolution r f, ForestConstraint' f t n s, MetadataSolution r m, Metadata m s)
type ForestConstraint'     f t n s   = (GeneralForest f t, TreeConstraint' t n s)
type TreeConstraint'         t n s   = (NodeConstraint' n s, ReferentialTree t n, BinaryTree t n, Show t)
type NodeConstraint'           n s   = (PreliminaryNode n s, EncodedNode n s, FinalNode n s, SeqConstraint' s)
type SeqConstraint'              s   = (EncodableDynamicCharacter s, Eq s, Show s, Bits s, Memoizable s, Memoizable (Element s), Show (Element s))
