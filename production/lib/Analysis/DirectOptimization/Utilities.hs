{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization.Utilities where

import Prelude hiding (length, filter)

import Control.Arrow ((***))
import Data.Matrix.NotStupid (Matrix, (!), nrows, ncols, setElem, zero, elementwise, getRow, matrix)
import Data.Bits
import Data.Vector (Vector, length, filter)
import Data.Maybe

import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Referential
import Bio.Phylogeny.Tree.Binary

import Bio.Sequence.Coded

type TreeConstraint t n s b = (Eq n, Network t n, NodeConstraint n s b, ReferentialTree t n, BinaryTree t n, Show t)
type NodeConstraint n s b = (PreliminaryNode n s, EncodedNode n s, SeqConstraint s b)
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

-- | Create a subtree matrix to find all sub nodes
getSubtrees''' :: TreeConstraint t n s b => t -> Subtrees
getSubtrees''' tree = fst $ innerSubtree tree zeroMatrix (root tree)
    where
        zeroMatrix = zero (numNodes tree) (numNodes tree)
        innerSubtree :: TreeConstraint t n s b => t -> Subtrees -> n -> (Subtrees, [n])
        innerSubtree inTree curSubtrees curNode
            | isLeaf curNode inTree = (curSubtrees, [curNode])
            | otherwise = 
                let
                    lowersubs = fmap (innerSubtree inTree curSubtrees) (children curNode inTree)
                    totalSubs = foldr sumSubs (zeroMatrix, []) lowersubs
                in (accum totalSubs curNode inTree, curNode : (snd totalSubs))

        sumMat = elementwise (+)

        sumSubs :: NodeConstraint n s b => (Subtrees, [n]) -> (Subtrees, [n]) -> (Subtrees, [n])
        sumSubs (struc1, list1) (struc2, list2) = (struc1 `sumMat` struc2, list1 ++ list2)

        accum :: TreeConstraint t n s b => (Subtrees, [n]) -> n -> t -> Subtrees
        accum (struc, nodes) curNode inTree = foldr (\n acc -> setElemSafe 1 (code n inTree, curCode) acc) struc nodes
            where curCode = code curNode inTree

-- | Create a subtree matrix to find all sub nodes
getSubtrees :: TreeConstraint t n s b => t -> Subtrees
getSubtrees tree = subtreeMatrix
  where
    n = numNodes tree
    subtreeMatrix = matrix n n omega
    omega :: (Int, Int) -> Int
    omega (i,j)
      | i == j              = 0
      | null childs         = 0
      | nodeJ `elem` childs = 1
      | otherwise           = maximum $ (\n -> subtreeMatrix ! (pointer n, j)) <$> childs
      where
        nodeI   = getNthNode tree i
        nodeJ   = getNthNode tree j
        childs  = children nodeI tree
        pointer = fromJust . flip code tree


-- | Helper function to grab a subtree from the node at the given position
grabSubtree :: TreeConstraint t n s b => t -> Maybe Int -> Subtrees -> t
grabSubtree inTree inPos subtreeMat
    | isNothing inPos = mempty
    | otherwise = 
        let positions = getRow (fromJust inPos) subtreeMat
        in foldr (\i acc -> acc `addNode` getNthNode inTree i) mempty positions
