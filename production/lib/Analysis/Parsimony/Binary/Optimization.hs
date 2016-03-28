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

{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Analysis.Parsimony.Binary.Optimization where

import Analysis.Parsimony.Binary.Internal
import Analysis.Parsimony.Binary.Fitch
import Analysis.Parsimony.Binary.DirectOptimization
--import Analysis.Parsimony.Binary.SequentialAlign

import Data.Maybe
import Data.Vector (Vector, ifoldr, (!))
import Data.Monoid

import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Tree.Node.Final
import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.CharacterAware
import Bio.Phylogeny.PhyloCharacter

-- | Unified function to perform both the first and second passes
allOptimization :: TreeConstraint t n s b => Double -> t -> t
--allOptimization _ inTree | trace ("allOptimization " ++ show (names inTree IM.! 63)) False = undefined
allOptimization weight inTree = 
    let 
        downPass = optimizationPreorder weight inTree
        upPass = optimizationPostorder downPass
    in upPass

-- | Optimization down pass warpper for recursion from root
optimizationPreorder :: TreeConstraint t n s b => Double -> t -> t
optimizationPreorder weight tree
    | isLeaf (root tree) tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let 
            newNode = setLocalCost 0.0 $ setTotalCost 0.0 (root tree)
            newTree = tree `update` [newNode]
        in newTree
    | leftOnly && rightOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            newNodes = (setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) (root tree)) : nodes1
        in tree `update` newNodes
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) (root tree)
            newNodes = myNode : nodes1
        in tree `update` newNodes
    | otherwise = 
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree 
            nodes2 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree
            myNode = preorderNodeOptimize weight (root tree) (head nodes1) (head nodes2) (characters tree)
            newNodes = myNode : (nodes1 ++ nodes2)
        in tree `update` newNodes

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal down pass that creates new rows without combining, making the algorithm faster
internalPreorder :: TreeConstraint t n s b => Double -> n -> t -> [n]
internalPreorder weight node tree 
    | isLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setTotalCost 0.0 $ setLocalCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) node
        in myNode : nodes1
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) node
        in myNode : nodes1
    | otherwise = 
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree 
            nodes2 = internalPreorder weight (fromJust $ leftChild node tree) tree
            myNode = preorderNodeOptimize weight node (head nodes1) (head nodes2) (characters tree)
        in myNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Wrapper for up pass recursion to deal with root
optimizationPostorder :: TreeConstraint t n s b => t -> t
optimizationPostorder tree 
    | isLeaf (root tree) tree = tree
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = 
        let nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree
        in tree `update` nodes1
    | leftOnly = 
        let nodes1 = internalPostorder (fromJust $ leftChild (root tree) tree) tree
        in tree `update` nodes1
    | otherwise = 
        let
            nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree
            nodes2 = internalPostorder (fromJust $ leftChild (root tree) tree) tree
        in tree `update` (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal up pass that performs most of the recursion
internalPostorder :: TreeConstraint t n s b => n -> t -> [n]
internalPostorder node tree 
    | isLeaf node tree = []
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = internalPostorder (fromJust $ rightChild node tree) tree
    | leftOnly = internalPostorder (fromJust $ leftChild node tree) tree
    | otherwise = 
        let 
            newNode = postorderNodeOptimize node (fromJust $ leftChild node tree) (fromJust $ rightChild node tree) (parent node tree) (characters tree)
            nodes1 = internalPostorder (fromJust $ rightChild node tree) tree
            nodes2 = internalPostorder (fromJust $ leftChild node tree) tree
        in newNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Wrapper function to preform optimization on a node (preorder)
preorderNodeOptimize :: NodeConstraint n s b => Double -> n -> n -> n -> Vector (PhyloCharacter s) -> n
preorderNodeOptimize weight curNode lNode rNode treeChars = 
    let foldResult = ifoldr chooseOptimization curNode treeChars
    in setTotalCost (totalCost foldResult + totalCost lNode + totalCost rNode) foldResult
    where
        chooseOptimization curPos curCharacter setNode
            | aligned curCharacter = 
                let (assign, temp, local) = preorderFitchBit weight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) curCharacter
                in addLocalCost local $ addTotalCost local $ addAlign assign $ addPreliminary assign setNode
            | otherwise = 
                let (ungapped, cost, gapped, leftGapped, rightGapped) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
                in addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode
                --let (ungapped, cost, gapped, leftGapped, rightGapped) = sequentialAlign (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
                --in addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode

        addPreliminary addVal inNode = addToField setPreliminary preliminary addVal inNode
        addAlign addVal inNode = addToField setAlign preliminaryAlign addVal inNode
        addTotalCost addVal node = setTotalCost (addVal + totalCost node) node
        addLocalCost addVal node = setLocalCost (addVal + localCost node) node

-- | Functionality to help add to node
addToField :: NodeConstraint n s b => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node

-- | Wrapper function to preform optimization on a node (postorder)
postorderNodeOptimize :: NodeConstraint n s b => n -> n -> n -> Maybe n -> Vector (PhyloCharacter s) -> n
postorderNodeOptimize curNode lNode rNode pNode treeChars 
    | isNothing pNode = error "No parent node on postorder traversal"
    | otherwise = ifoldr chooseOptimization curNode treeChars
    where
        chooseOptimization i curCharacter setNode
            | aligned curCharacter = 
                let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign (fromJust pNode) ! i) (temporary curNode ! i) curCharacter
                in addToField setFinal final finalAssign setNode
            | otherwise = setNode
