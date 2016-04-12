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

{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes #-}

module Analysis.Parsimony.Binary.Optimization where

import Analysis.Parsimony.Binary.Internal
import Analysis.Parsimony.Binary.Fitch
import Analysis.Parsimony.Binary.DirectOptimization
import Analysis.Parsimony.Binary.SequentialAlign

import Data.Maybe
import Data.Vector (Vector, ifoldl', ifoldr, (!))
import Data.Monoid


import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Solution.Class
import Bio.PhyloGraph.Solution.Metadata
import Bio.PhyloGraph.Tree.Binary
import Bio.PhyloGraph.Node (Node)
import Bio.PhyloGraph.Node.Final
import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Node.Encoded
import Bio.Metadata

--import Debug.Trace

-- | Additional wrapper to optimize over a solution
solutionOptimization :: SolutionConstraint' r f t n s m => Double -> r -> r
solutionOptimization weight inSolution = setForests inSolution (map (graphOptimization weight meta) (forests inSolution))
    where
        meta = metadata inSolution

-- | Mapping function to optimize over a forest
graphOptimization :: (ForestConstraint' f t n s, Metadata m s) => Double -> Vector m -> f -> f
graphOptimization weight meta inGraph = setTrees inGraph (map (allOptimization weight meta) (trees inGraph))

-- | Unified function to perform both the first and second passes
allOptimization :: (TreeConstraint' t n s, Metadata m s) => Double -> Vector m -> t -> t
--allOptimization _ inTree | trace ("allOptimization " ++ show (names inTree IM.! 63)) False = undefined
allOptimization weight meta inTree =
    let
        downPass = optimizationPreorder weight inTree meta
        upPass = optimizationPostorder downPass meta
    in upPass

-- | Optimization down pass warpper for recursion from root
optimizationPreorder :: (TreeConstraint' t n s, Metadata m s) => Double -> t -> Vector m -> t
optimizationPreorder weight tree meta
    | isLeaf (root tree) tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let
            newNode = setLocalCost 0.0 $ setTotalCost 0.0 (root tree)
            newTree = tree `update` [newNode]
        in newTree
    | leftOnly && rightOnly = tree --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            newNodes = (setTemporary (getTemporary carryNode) $ setAlign (getPreliminaryAlign carryNode)
                        $ setPreliminary (getPreliminary carryNode) $ setTotalCost (getTotalCost carryNode) $ setLocalCost (getLocalCost carryNode) (root tree)) : nodes1
        in tree `update` newNodes
    | leftOnly =
        let
            nodes1 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (getTemporary carryNode) $ setAlign (getPreliminaryAlign carryNode)
                        $ setPreliminary (getPreliminary carryNode) $ setTotalCost (getTotalCost carryNode) $ setLocalCost (getLocalCost carryNode) (root tree)
            newNodes = myNode : nodes1
        in tree `update` newNodes
    | otherwise =
        let
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree meta
            nodes2 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree meta
            myNode = preorderNodeOptimize weight (root tree) (head nodes1) (head nodes2) meta
            newNodes = myNode : (nodes1 ++ nodes2)
        in tree `update` newNodes

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal down pass that creates new rows without combining, making the algorithm faster
internalPreorder :: (TreeConstraint' t n s, Metadata m s) => Double -> n -> t -> Vector m -> [n]
internalPreorder weight node tree meta
    | isLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setTotalCost 0.0 $ setLocalCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = [] --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (getTemporary carryNode) $ setAlign (getPreliminaryAlign carryNode)
                        $ setPreliminary (getPreliminary carryNode) $ setTotalCost (getTotalCost carryNode) $ setLocalCost (getLocalCost carryNode) node
        in myNode : nodes1
    | leftOnly =
        let
            nodes1 = internalPreorder weight (fromJust $ leftChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (getTemporary carryNode) $ setAlign (getPreliminaryAlign carryNode)
                        $ setPreliminary (getPreliminary carryNode) $ setTotalCost (getTotalCost carryNode) $ setLocalCost (getLocalCost carryNode) node
        in myNode : nodes1
    | otherwise =
        let

            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree meta
            nodes2 = internalPreorder weight (fromJust $ leftChild node tree) tree meta
            myNode = preorderNodeOptimize weight node (head nodes1) (head nodes2) meta
        in myNode : (nodes1 ++ nodes2)

        where
            leftOnly  = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Wrapper for up pass recursion to deal with root
optimizationPostorder :: (TreeConstraint' t n s, Metadata m s) => t -> Vector m -> t
optimizationPostorder tree meta
    | isLeaf (root tree) tree = tree
    | rightOnly && leftOnly = tree --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly =
        let nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree meta
        in tree `update` nodes1
    | leftOnly =
        let nodes1 = internalPostorder (fromJust $ leftChild (root tree) tree) tree meta
        in tree `update` nodes1
    | otherwise =
        let
            nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree meta
            nodes2 = internalPostorder (fromJust $ leftChild (root tree) tree) tree meta
        in tree `update` (nodes1 ++ nodes2)

        where
            leftOnly  = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal up pass that performs most of the recursion
internalPostorder :: (TreeConstraint' t n s, Metadata m s) => n -> t -> Vector m -> [n]
internalPostorder node tree meta
    | isLeaf node tree = []
    | rightOnly && leftOnly = [] --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = internalPostorder (fromJust $ rightChild node tree) tree meta
    | leftOnly  = internalPostorder (fromJust $ leftChild node tree) tree meta
    | otherwise =
        let newNode = postorderNodeOptimize node (fromJust $ leftChild node tree) (fromJust $ rightChild node tree) (parent node tree) meta
            nodes1  = internalPostorder (fromJust $ rightChild node tree) tree meta
            nodes2  = internalPostorder (fromJust $ leftChild node tree) tree meta
        in newNode : (nodes1 ++ nodes2)

        where
            leftOnly  = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree


-- | Wrapper function to preform optimization on a node (preorder)
preorderNodeOptimize :: (NodeConstraint' n s, Metadata m s) => Double -> n -> n -> n -> Vector m -> n
preorderNodeOptimize weight curNode lNode rNode meta = setTotalCost summedTotalCost res
    where
        summedTotalCost = sum $ getTotalCost <$> [res,lNode,rNode] --getTotalCost res + getTotalCost lNode + getTotalCost rNode
        res             = ifoldr chooseOptimization curNode meta

        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization curPos curCharacter setNode
            -- TODO: Compiler error maybe below with comment structuers and 'lets'
            | getIgnored curCharacter = setNode
            | getAligned curCharacter && not (getAdditive curCharacter) =
                let (assign, temp, local) = preorderFitchBit weight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) curCharacter
                in addLocalCost (local * curWeight) $ addTotalCost (local * curWeight) $ addAlign assign $ addPreliminary assign setNode
            | otherwise =
                let (ungapped, cost, gapped, leftGapped, rightGapped) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) curCharacter
                in addLocalCost (cost * curWeight) $ addTotalCost (cost * curWeight) $ addAlign gapped $ addPreliminary ungapped setNode

                where curWeight = getWeight curCharacter

                -- getForAlign returns a node, either encoded, getPreliminary or getPreliminary align. It's in Analysis.Parsimony.Binary.Internal
                -- the return type is a vector of encoded sequences,
                -- where an EncodedSeq (encoded sequence) is a maybe vector of some type from Bio/Sequence/Coded.hs
                --let (ungapped, cost, gapped, leftGapped, rightGapped) = sequentialAlign (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
                --in  addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode

        addPreliminary addVal inNode = addToField setPreliminary getPreliminary      addVal inNode
        addAlign       addVal inNode = addToField setAlign       getPreliminaryAlign addVal inNode
        addTotalCost   addVal node   = setTotalCost (addVal + getTotalCost node) node
        addLocalCost   addVal node   = setLocalCost (addVal + getLocalCost node) node

-- | addToField takes in a setter fn, a getter fn, a value and a node.
-- It then gets the related value from the node, adds to it the passed value,
-- and sets that value on the node. It returns a new node with the newly computed value set.
addToField :: NodeConstraint' n s => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node

-- | Wrapper function to preform optimization on a node (postorder)
postorderNodeOptimize :: (NodeConstraint' n s, Metadata m s) => n -> n -> n -> Maybe n -> Vector m -> n
postorderNodeOptimize curNode lNode rNode pNode meta
    | isNothing pNode = error "No parent node on postorder traversal"
    | otherwise       = ifoldr chooseOptimization curNode meta
    where
        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization i curCharacter setNode
            | getAligned curCharacter =
                let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign (fromJust pNode) ! i) (getTemporary curNode ! i) curCharacter
                in addToField setFinal getFinal finalAssign setNode
            | otherwise = setNode
