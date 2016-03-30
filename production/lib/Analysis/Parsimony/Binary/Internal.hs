-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Optimization
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- General wrapper for all forms of optimization on binary trees
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}

module Analysis.Parsimony.Binary.Internal where

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.Node.Final
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Network.Subsettable
import Bio.Sequence.Coded
import Bio.Phylogeny.Tree.CharacterAware
import Data.Bits
import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
import Data.Maybe
import Data.Vector           (Vector)


type TreeConstraint t n s b = (Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n, CharacterTree t s)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b, Show n, Eq n, FinalNode n s)
type SeqConstraint s b = (CodedSequence s, Eq s, CharConstraint b, Show s, Bits s, Monoid s)
type CharConstraint b = (Bits b, Eq b, CodedChar b, Show b)
type Subtrees = Matrix Int

setElemSafe :: (Num a) => a -> (Maybe Int, Maybe Int) -> Matrix a -> Matrix a
setElemSafe value (row, col) matrix
    | isNothing row || isNothing col = error "Attempt to set matrix out of bounds using a Nothing dimension"
    | fromJust row >= nrows matrix || fromJust col >= ncols matrix || fromJust row < 0 || fromJust col < 0 = error "Attempt to set matrix value out of bounds"
    | otherwise = setElem value (fromJust row, fromJust col) matrix

-- | Simple function to get the aligned if available, the encoded if not
getForAlign :: NodeConstraint n s b => n -> Vector s
getForAlign node 
    | null (preliminaryAlign node) && null (preliminary node) = encoded node
    | null $ preliminaryAlign node                            = preliminary node
    | otherwise                                               = preliminaryAlign node 

