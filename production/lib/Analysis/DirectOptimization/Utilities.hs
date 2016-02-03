{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization.Utilities where

import Prelude hiding (length, filter)

import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
import Data.Bits
import Data.Vector (Vector, length, filter)
import Data.Maybe

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Network.Subsettable

import Bio.Sequence.Coded

--import Debug.Trace

type TreeConstraint t n s b = (Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n, Show t, SubsettableNetwork t n)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b, Show n, Eq n)
type SeqConstraint s b = (CodedSequence s b, Eq s, CharConstraint b, Show s)
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
    | (null $ preliminaryAlign node) && (null $ preliminary node) = encoded node
    | null $ preliminaryAlign node = preliminary node
    | otherwise = preliminaryAlign node 

-- | Screening function to get sequences that are alignable from two nodes
checkForAlign :: NodeConstraint n s b => n -> n -> (Vector s, Vector s)
checkForAlign node1 node2
    | getMatch pa1 pa2 && checkLens pa1 pa2 = (screen pa1, screen pa2)
    | getMatch p1 p2 && checkLens p1 p2 = (screen p1, screen p2)
    | getMatch e1 e2 && checkLens e1 e2 = (screen e1, screen e2)
    | otherwise = (mempty, mempty)
        where
            pa1 = preliminaryAlign node1
            pa2 = preliminaryAlign node2
            p1 = preliminary node1
            p2 = preliminary node2
            e1 = encoded node1
            e2 = encoded node2
            getMatch s1 s2 = (not $ null s1) && (not $ null s2)
            screen = filter isEmpty
            checkLens s1 s2 = (length $ screen s1) == (length $ screen s2)
