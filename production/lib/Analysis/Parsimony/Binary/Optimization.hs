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
--import Analysis.Parsimony.Binary.SequentialAlign

import Data.Maybe
import Data.Vector (Vector, ifoldl', ifoldr, (!))
import Data.Monoid


import Bio.Phylogeny.Forest
import Bio.Phylogeny.Network
import Bio.Phylogeny.Solution.Class
import Bio.Phylogeny.Solution.Metadata
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Tree.Node.Final
import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.CharacterAware
import Bio.Metadata.Class (InternalMetadata(..))

--import Debug.Trace

-- | Additional wrapper to optimize over a solution
solutionOptimization :: SolutionConstraint r f t n s b m => Double -> r -> r
--solutionOptimization _ _ | trace "solutionOptimization" False = undefined
solutionOptimization weight inSolution = setForests inSolution (map (graphOptimization weight meta) (forests inSolution))
    where
        meta = metadata inSolution

-- | Mapping function to optimize over a forest
graphOptimization :: (ForestConstraint f t n s b, Metadata m s) => Double -> Vector m -> f -> f
graphOptimization weight meta inGraph = setTrees inGraph (map (allOptimization weight meta) (trees inGraph))

-- | Unified function to perform both the first and second passes
allOptimization :: (TreeConstraint t n s b, Metadata m s) => Double -> Vector m -> t -> t
--allOptimization _ _ inTree | trace ("allOptimization ") False = undefined
allOptimization weight meta inTree = 
    let 
        downPass = optimizationPreorder weight inTree meta
        upPass = optimizationPostorder downPass meta
    in upPass

-- | Optimization down pass warpper for recursion from root
-- TODO: add a warning here if an internal node has no children (for all traversals)
optimizationPreorder :: (TreeConstraint t n s b, Metadata m s) => Double -> t -> Vector m -> t
--optimizationPreorder weight tree meta | trace "optimizationPreorder" False = undefined
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
            newNodes = (setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) (root tree)) : nodes1
        in tree `update` newNodes
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) (root tree)
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
internalPreorder :: (TreeConstraint t n s b, Metadata m s) => Double -> n -> t -> Vector m -> [n]
--internalPreorder weight node tree meta | trace "internalPreorder" False = undefined
internalPreorder weight node tree meta
    | isLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setTotalCost 0.0 $ setLocalCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = [] --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) node
        in myNode : nodes1
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setTotalCost (totalCost carryNode) $ setLocalCost (localCost carryNode) node
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
optimizationPostorder :: (TreeConstraint t n s b, Metadata m s) => t -> Vector m -> t
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
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal up pass that performs most of the recursion
internalPostorder :: (TreeConstraint t n s b, Metadata m s) => n -> t -> Vector m -> [n]
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
preorderNodeOptimize :: (NodeConstraint n s b, Metadata m s) => Double -> n -> n -> n -> Vector m -> n
--preorderNodeOptimize weight curNode lNode rNode meta | trace ("preorderNodeOptimize" ++ show curNode ++ show meta) False = undefined
preorderNodeOptimize weight curNode lNode rNode meta = setTotalCost summedTotalCost res 
    where
        summedTotalCost = sum $ totalCost <$> [res,lNode,rNode] --totalCost res + totalCost lNode + totalCost rNode
        res             = ifoldr chooseOptimization curNode meta

--        chooseOptimization :: NodeConstraint n s b => Int -> m -> n -> n 
        --chooseOptimization curPos curCharacter setNode | trace ("chooseOptimization" ++ show curCharacter) False = undefined
        chooseOptimization curPos curCharacter setNode
            -- TODO: Compiler error maybe below with comment structuers and 'lets'
            | aligned curCharacter = --trace "calling fitch" $ 
                let (assign, temp, local) = {- trace (show curCharacter) $ -} preorderFitchBit weight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) curCharacter
                in addLocalCost local $ addTotalCost local $ addAlign assign $ addPreliminary assign setNode
            | otherwise = --trace "calling DO" $
                let (ungapped, cost, gapped, leftGapped, rightGapped) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) curCharacter
                in addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode
                --let (ungapped, cost, gapped, leftGapped, rightGapped) = sequentialAlign (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
                --in addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode

                -- getForAlign returns a node, either encoded, preliminary or preliminary align. It's in Analysis.Parsimony.Binary.Internal
                -- the return type is a vector of encoded sequences, 
                -- where an EncodedSeq (encoded sequence) is a maybe vector of some type from Bio/Sequence/Coded.hs
                --let (!ungapped, !cost, !gapped, !leftGapped, !rightGapped) = sequentialAlign (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
                --in  trace (show ungapped ++ " " ++ (show gapped)) $ addLocalCost cost $ addTotalCost cost $ addAlign gapped $ addPreliminary ungapped setNode

      {-  chooseOptimization' !setNode !curPos !curCharacter = addLocalCost cost . addTotalCost cost . addAlign gapped $ addPreliminary ungapped setNode
            where
                -- getForAlign returns a node, either encoded, preliminary or preliminary align. It's in Analysis.Parsimony.Binary.Internal
                -- the return type is a vector of encoded sequences, 
                -- where an EncodedSeq (encoded sequence) is a maybe vector of some type from Bio/Sequence/Coded.hs
                (!ungapped, !cost, !gapped, _leftGapped, _rightGapped) = sequentialAlign (getForAlign lNode ! curPos) (getForAlign rNode ! curPos)
      -}

        addPreliminary addVal inNode = addToField setPreliminary preliminary      addVal inNode
        addAlign       addVal inNode = addToField setAlign       preliminaryAlign addVal inNode
        addTotalCost   addVal node   = setTotalCost (addVal + totalCost node) node
        addLocalCost   addVal node   = setLocalCost (addVal + localCost node) node

-- I think this is the problem. In ll. 173 & 175 it's adding the new Vectors of bits to something that's empty? See <> in l. 183
-- | Functionality to help add to node
addToField :: NodeConstraint n s b => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node

-- | Wrapper function to preform optimization on a node (postorder)
postorderNodeOptimize :: (NodeConstraint n s b, Metadata m s) => n -> n -> n -> Maybe n -> Vector m -> n
postorderNodeOptimize curNode lNode rNode pNode meta
    | isNothing pNode = error "No parent node on postorder traversal"
    | otherwise = ifoldr chooseOptimization curNode meta
    where
        chooseOptimization i curCharacter setNode
            | aligned curCharacter = 
                let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign (fromJust pNode) ! i) (temporary curNode ! i) curCharacter
                in addToField setFinal final finalAssign setNode
            | otherwise = setNode
