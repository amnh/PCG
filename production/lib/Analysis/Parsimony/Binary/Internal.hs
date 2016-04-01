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

import Bio.Metadata.Class (InternalMetadata)
import Bio.Phylogeny.Forest
import Bio.Phylogeny.Solution.Class
import Bio.Phylogeny.Solution.Metadata
import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.Node.Final
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Network.Subsettable
import Bio.Sequence.Coded
import Data.Bits
import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
import Data.Maybe
import Data.Vector           (Vector)

type SolutionConstraint  r f t n s b m = (Solution r f, ForestConstraint f t n s b, MetadataSolution r m, Metadata m s)
type SolutionConstraint' r f t n s   m = (Solution r f, ForestConstraint' f t n s, MetadataSolution r m, Metadata m s)
type ForestConstraint      f t n s b   = (Forest f t, TreeConstraint t n s b)
type ForestConstraint'     f t n s     = (Forest f t, TreeConstraint' t n s)
type TreeConstraint          t n s b   = (Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n)
type TreeConstraint'         t n s     = (Network t n, NodeConstraint' n s, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n)
type NodeConstraint            n s b   = (PreliminaryNode n s, EncodedNode n s, FinalNode n s, SeqConstraint s b, Show n, Eq n)
type NodeConstraint'           n s     = (PreliminaryNode n s, EncodedNode n s, FinalNode n s, SeqConstraint' s)
type SeqConstraint               s b   = (CodedSequence s, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
type SeqConstraint'              s     = (CodedSequence s, Eq s, Show s, Bits s, Monoid s)
type CharConstraint                b   = (Bits b, Eq b, Show b)
type Subtrees                          = Matrix Int
type Metadata m s                      = (InternalMetadata m s, Show m)


setElemSafe :: (Num a) => a -> (Maybe Int, Maybe Int) -> Matrix a -> Matrix a
setElemSafe value (row, col) matrix
    | isNothing row || isNothing col = error "Attempt to set matrix out of bounds using a Nothing dimension"
    | fromJust row >= nrows matrix || fromJust col >= ncols matrix || fromJust row < 0 || fromJust col < 0 = error "Attempt to set matrix value out of bounds"
    | otherwise = setElem value (fromJust row, fromJust col) matrix

-- | Simple function to get the aligned if available, the encoded if not
getForAlign :: (PreliminaryNode n s, EncodedNode n s, SeqConstraint' s) => n -> Vector s
getForAlign node 
    | null (preliminaryAlign node) && null (preliminary node) = encoded node
    | null $ preliminaryAlign node                            = preliminary node
    | otherwise                                               = preliminaryAlign node 

gapChar :: Int -> BitVector
gapChar alphLen = setBit (bitVec alphLen 0) 0

