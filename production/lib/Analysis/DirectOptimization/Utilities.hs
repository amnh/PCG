{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization.Utilities where

import Prelude hiding (null)
import Data.Matrix (Matrix, nrows, ncols, setElem)
import Data.Bits
import Data.Vector

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Binary

import Bio.Sequence.Coded

type TreeConstraint t n s b = (Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b)
type SeqConstraint s b = (CodedSequence s b, Eq s, CharConstraint b)
type CharConstraint b = (Bits b, Eq b, CodedChar b)

setElemSafe :: (Num a) => a -> (Int, Int) -> Matrix a -> Matrix a
setElemSafe value (row, col) matrix
    | row >= nrows matrix || col >= ncols matrix || row < 0 || col < 0 = error "Attempt to set matrix value out of bounds"
    | otherwise = setElem value (row, col) matrix

-- | Simple function to get the aligned if available, the encoded if not
getForAlign :: NodeConstraint n s b => n -> Vector s
getForAlign node 
    | (null $ preliminaryAlign node) && (null $ preliminary node) = encoded node
    | null $ preliminaryAlign node = preliminary node
    | otherwise = preliminaryAlign node 