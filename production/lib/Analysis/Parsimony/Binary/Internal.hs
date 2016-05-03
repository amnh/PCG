-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Internal
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
{-# LANGUAGE ConstraintKinds #-}

module Analysis.Parsimony.Binary.Internal where

import Bio.Metadata
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Node.Encoded
import Bio.PhyloGraph.Node.Final
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Tree.Referential
import Bio.PhyloGraph.Tree.Binary
import Bio.PhyloGraph.Tree.Rose
import Bio.PhyloGraph.Network.Subsettable
import Bio.Sequence.Coded
import Data.Bits
import Data.Function.Memoize
import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
import Data.Maybe
import Data.Vector           (Vector)

type SolutionConstraint  r f t n s b m = (GeneralSolution r f, ForestConstraint f t n s b, MetadataSolution r m, Metadata m s)
type SolutionConstraint' r f t n s   m = (GeneralSolution r f, ForestConstraint' f t n s, MetadataSolution r m, Metadata m s)
type ForestConstraint      f t n s b   = (GeneralForest f t, TreeConstraint t n s b)
type ForestConstraint'     f t n s     = (GeneralForest f t, TreeConstraint' t n s)
type TreeConstraint          t n s b   = (Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n)
type TreeConstraint'         t n s     = (Network t n, NodeConstraint' n s, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n, RoseTree t n)
type NodeConstraint            n s b   = (PreliminaryNode n s, EncodedNode n s, FinalNode n s, SeqConstraint s b, Show n, Eq n)
type NodeConstraint'           n s     = (PreliminaryNode n s, EncodedNode n s, FinalNode n s, SeqConstraint' s)
type SeqConstraint               s b   = (EncodableDynamicCharacter s, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
type SeqConstraint'              s     = (EncodableDynamicCharacter s, Eq s, Show s, Bits s, Monoid s, Memoizable s)
type CharConstraint                b   = (Bits b, Eq b, Show b)
type Subtrees                          = Matrix Int


setElemSafe :: (Num a) => a -> (Maybe Int, Maybe Int) -> Matrix a -> Matrix a
setElemSafe value (row, col) matrix
    | isNothing row || isNothing col = error "Attempt to set matrix out of bounds using a Nothing dimension"
    | fromJust row >= nrows matrix || fromJust col >= ncols matrix || fromJust row < 0 || fromJust col < 0 = error "Attempt to set matrix value out of bounds"
    | otherwise = setElem value (fromJust row, fromJust col) matrix

-- | Simple function to get the aligned if available, the encoded if not
getForAlign :: (PreliminaryNode n s, EncodedNode n s, SeqConstraint' s) => n -> Vector s
getForAlign node 
    | null (getPreliminaryAlign node) && null (getPreliminary node) = getEncoded node
    | null $ getPreliminaryAlign node                            = getPreliminary node
    | otherwise                                               = getPreliminaryAlign node 

