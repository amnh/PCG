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

{-# LANGUAGE ConstraintKinds #-}

module Analysis.Parsimony.Binary.Optimization where

import Analysis.Parsimony.Binary.Internal

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
            myNode = preorderBitOps weight (root tree) (head nodes1) (head nodes2) (characters tree)
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
            myNode = preorderBitOps weight node (head nodes1) (head nodes2) (characters tree)
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
            newNode = postorderBitOps node (fromJust $ leftChild node tree) (fromJust $ rightChild node tree) (parent node tree) (characters tree)
            nodes1 = internalPostorder (fromJust $ rightChild node tree) tree
            nodes2 = internalPostorder (fromJust $ leftChild node tree) tree
        in newNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree


