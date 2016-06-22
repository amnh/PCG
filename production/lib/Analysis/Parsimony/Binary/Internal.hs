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
{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts #-}

module Analysis.Parsimony.Binary.Internal where

import Analysis.Parsimony.Binary.Constraints
import Analysis.Parsimony.Binary.DirectOptimization
import Analysis.Parsimony.Binary.Fitch
import Bio.Metadata
import Bio.PhyloGraph.Forest
import Bio.PhyloGraph.Network
import Bio.PhyloGraph.Node ()
import Bio.PhyloGraph.Node.Encoded
import Bio.PhyloGraph.Node.Final
import Bio.PhyloGraph.Node.Preliminary
import Bio.PhyloGraph.Solution
import Bio.PhyloGraph.Tree.Binary
import Bio.PhyloGraph.Tree.Rose
import Data.Matrix.NotStupid (Matrix, nrows, ncols, setElem)
import Data.Maybe
import Data.Monoid
import Data.Vector           (Vector, ifoldr, (!))

-- | Top level binary optimization wrapper to optimize over a solution
-- Takes in an overall weight and a solution
-- Returns a solution with any relevant values assigned (root cost, node assignments, etc. depending on optimization types).
-- Calls 'graphOptimization'.
solutionOptimization :: SolutionConstraint' r f t n s m => Double -> r -> r
solutionOptimization weighting inSolution = outForests 
    where
        meta = getMetadata inSolution
        outForests = setForests inSolution $ fmap (graphOptimization weighting meta) (getForests inSolution)

-- | Mapping function to optimize over a forest
-- Takes in an overall weight, a vector of metadata, and a forest
-- Returns a forest with relevant values assigned.
-- Calls 'allOptimization'.
graphOptimization :: (ForestConstraint' f t n s, Metadata m s) => Double -> Vector m -> f -> f
graphOptimization weighting meta inGraph = setTrees inGraph $ fmap (allOptimization weighting meta) (trees inGraph)

-- | Unified function to perform both the preorder and postorder passes (as relevant)
-- Takes in an overall weight, a vector of metadata, and a tree.
-- Returns a tree with values assigned.
-- This actually calls the preorder and postorder optimizations.
allOptimization :: (TreeConstraint' t n s, Metadata m s) => Double -> Vector m -> t -> t
allOptimization weighting meta inTree =
    let
        downPass = treeOptimizePreorder weighting inTree meta
        upPass   = treeOptimizePostorder downPass meta
    in upPass

-- | Optimization preorder wrapper to perform relevant algorithm at all nodes
-- Takes in an overall weight, a tree, and a vector of metadata
-- Returns a tree with values assigned
-- Correctly handles roots, leaves, and nodes with only one child
treeOptimizePreorder :: (TreeConstraint' t n s, Metadata m s) => Double -> t -> Vector m -> t
treeOptimizePreorder weighting tree meta 
    | nodeIsLeaf (root tree) tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let
            newNode = setLocalCost 0.0 $ setTotalCost 0.0 (root tree)
            newTree = tree `update` [newNode]
        in newTree
    | noLeftChild && noRightChild = tree 
    | noLeftChild = -- if there is only one child, continue recursion down and resolve
        let
            nodes1    = treeInternalPreorderTraversal weighting (fromJust $ rightChild (root tree) tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            newNodes  = ( setTemporary   (getTemporary        carryNode)
                        . setAlign       (getPreliminaryAlign carryNode)
                        . setPreliminary (getPreliminary      carryNode)
                        . setTotalCost   (getTotalCost        carryNode)
                        . setLocalCost   (getLocalCost        carryNode)
                        $ root tree
                        ) : nodes1
        in tree `update` newNodes
    | noRightChild =
        let
            nodes1    = treeInternalPreorderTraversal weighting (fromJust $ leftChild (root tree) tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode    = setTemporary   (getTemporary        carryNode)
                      . setAlign       (getPreliminaryAlign carryNode)
                      . setPreliminary (getPreliminary      carryNode)
                      . setTotalCost   (getTotalCost        carryNode)
                      . setLocalCost   (getLocalCost        carryNode)
                      $ root tree
            newNodes  = myNode : nodes1
        in tree `update` newNodes
    | otherwise =
        let
            nodes1   = treeInternalPreorderTraversal weighting (fromJust $ rightChild (root tree) tree) tree meta
            nodes2   = treeInternalPreorderTraversal weighting (fromJust $ leftChild (root tree) tree) tree meta
            myNode   = nodeOptimizePreorder weighting (root tree) (head nodes1) (head nodes2) meta
            newNodes = myNode : (nodes1 ++ nodes2)
        in tree `update` newNodes

    where
        noRightChild = isNothing $ rightChild (root tree) tree
        noLeftChild  = isNothing $ leftChild  (root tree) tree

-- | Internal preorder optimization pass
-- takes in a weight, a current node, a tree, and a vector of metadata
-- returns a list of nodes that have had changes applied to them
-- By using this node accumulation scheme, we save some complexity over simply always dealing with a tree
treeInternalPreorderTraversal :: (TreeConstraint' t n s, Metadata m s) => Double -> n -> t -> Vector m -> [n]
treeInternalPreorderTraversal weighting node tree meta
    | nodeIsLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setTotalCost 0.0 $ setLocalCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = [] --error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let
            nodes1    = treeInternalPreorderTraversal weighting (fromJust $ rightChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode    = setTemporary   (getTemporary        carryNode)
                      . setAlign       (getPreliminaryAlign carryNode)
                      . setPreliminary (getPreliminary      carryNode)
                      . setTotalCost   (getTotalCost        carryNode)
                      . setLocalCost   (getLocalCost        carryNode)
                      $ node
        in myNode : nodes1
    | leftOnly =
        let
            nodes1    = treeInternalPreorderTraversal weighting (fromJust $ leftChild node tree) tree meta -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode    = setTemporary   (getTemporary        carryNode)
                      . setAlign       (getPreliminaryAlign carryNode)
                      . setPreliminary (getPreliminary      carryNode)
                      . setTotalCost   (getTotalCost        carryNode)
                      . setLocalCost   (getLocalCost        carryNode)
                      $ node
        in myNode : nodes1
    | otherwise =
        let

            nodes1 = treeInternalPreorderTraversal weighting (fromJust $ rightChild node tree) tree meta
            nodes2 = treeInternalPreorderTraversal weighting (fromJust $ leftChild node tree) tree meta
            myNode = nodeOptimizePreorder weighting node (head nodes1) (head nodes2) meta
        in myNode : (nodes1 ++ nodes2)

        where
            leftOnly  = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Wrapper for the postorder
-- Takes in a tree and a vector of metadata,
-- returns a tree with relevant nodes assigned.
-- This wrapper allows us to deal correctly with root passing to postorder algorithms
treeOptimizePostorder :: (TreeConstraint' t n s, Metadata m s) => t -> Vector m -> t
treeOptimizePostorder tree meta = tree `update` treeInternalPostorderTraversal (root tree) tree meta

-- | Internal postorder pass that does the main recursion
-- Takes in a current node, the tree, and a vector of metadata;
-- returns a list of nodes that have been updated.
-- As in the preorder, this method saves on some time complexity.
treeInternalPostorderTraversal :: (TreeConstraint' t n s, Metadata m s) => n -> t -> Vector m -> [n]
treeInternalPostorderTraversal node tree meta  = 
  case children' of
      left:right:_ -> nodeOptimizePostorder node left right (parent node tree) meta : result
      _            -> result
  where
      children' = children node tree
      result    = concatMap (\x -> treeInternalPostorderTraversal x tree meta) children'

-- | Wrapper function to preform optimization on a node during the preorder pass
-- Essentially a map over a decision function that selects and performs the right optimization for each character.
-- Takes in an overall weight, a current node, the left child, the right child, and a vector of metadata
-- Outputs a node with the correct sequences and costs assigned.
nodeOptimizePreorder :: (NodeConstraint' n s, Metadata m s) => Double -> n -> n -> n -> Vector m -> n
nodeOptimizePreorder weighting curNode lNode rNode meta = summedTotalCost `setTotalCost` res
    where
        summedTotalCost = sum [getLocalCost res, getTotalCost lNode, getTotalCost rNode]
        res             = ifoldr chooseOptimization curNode meta

        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization curPos metadataStructure setNode
            -- TODO: Compiler error maybe below with comment structures and 'lets'
            | getIgnored metadataStructure = setNode
            | getType metadataStructure == Fitch =
                let (assign, temp, local) = preorderFitchBit curWeight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) metadataStructure
                in addTemporary temp
                 . addLocalCost (local * curWeight * weighting)
--                 . addTotalCost (local * curWeight * weighting)
                 . addAlign assign
                 $ addPreliminary assign setNode
            | getType metadataStructure == DirectOptimization =
                let (ungapped, cost, gapped, _, _) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) $ getCosts metadataStructure
                in addLocalCost (cost * curWeight * weighting)
--                 . addTotalCost (cost * curWeight * weighting)
                 . addAlign gapped
                 $ addPreliminary ungapped setNode
            | otherwise = error "Unrecognized optimization type"

                where curWeight = getWeight metadataStructure

        addPreliminary addVal node = addToField setPreliminary getPreliminary      addVal node
        addAlign       addVal node = addToField setAlign       getPreliminaryAlign addVal node
        addTemporary   addVal node = addToField setTemporary   getTemporary        addVal node
--        addTotalCost   addVal node = setTotalCost (addVal + getTotalCost node) node
        addLocalCost   addVal node = setLocalCost (addVal + getLocalCost node) node

-- | Wrapper function to perform optimization on a node during the postorder pass.
-- As in the preorder, it selects an optimization for each character, then groups the optimized characters together and assigns them to the node.
-- Takes in a current node, left child, right child, parent node, and vector of metadata,
-- returns a node with everything assigned.
nodeOptimizePostorder :: (NodeConstraint' n s, Metadata m s) => n -> n -> n -> Maybe n -> Vector m -> n
nodeOptimizePostorder curNode lNode rNode pNode meta
    | isNothing pNode = curNode --error "No parent node on postorder traversal"
    | otherwise       = ifoldr chooseOptimization curNode meta
    where
        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization i metadataStructure setNode
            | getType metadataStructure == Fitch =
                let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign (fromJust pNode) ! i) (getTemporary curNode ! i) metadataStructure
                in addToField setFinal getFinal finalAssign setNode
            | getType metadataStructure == DirectOptimization =  --TODO: do we grab the gapped or not?
                let (final, _, finalAligned, _, _) = naiveDO (getForAlign curNode ! i) (getForAlign (fromJust pNode) ! i) $ getCosts metadataStructure
                in addToField setFinal getFinal final
                 $ addToField setFinalGapped getFinalGapped finalAligned setNode
            | otherwise = error "Unrecognized optimization type"

setElemSafe :: (Num a) => a -> (Maybe Int, Maybe Int) -> Matrix a -> Matrix a
setElemSafe value (row, col) matrix
    | isNothing row || isNothing col = error "Attempt to set matrix out of bounds using a Nothing dimension"
    | fromJust row >= nrows matrix || fromJust col >= ncols matrix || fromJust row < 0 || fromJust col < 0 = error "Attempt to set matrix value out of bounds"
    | otherwise = setElem value (fromJust row, fromJust col) matrix

-- | getForAlign returns the sequences from a node, where the node type is either 'EncodedNode' or 'PreliminaryNode'.
-- preliminary alignment 
getForAlign :: (PreliminaryNode n s, EncodedNode n s, SeqConstraint' s) => n -> Vector s
getForAlign node
    | null (getPreliminaryAlign node) && null (getPreliminary node) = getEncoded node
    | null $ getPreliminaryAlign node                               = getPreliminary node
    | otherwise                                                     = getPreliminaryAlign node

-- | addToField takes in a setter fn, a getter fn, a value and a node.
-- It then gets the related value from the node, adds to it the passed value,
-- and sets that value on the node. It returns a new node with the newly computed value set.
addToField :: NodeConstraint' n s => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node
