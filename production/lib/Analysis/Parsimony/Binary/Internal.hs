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
import Bio.Character.Dynamic.Coded
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

-- !!!TODO: Remove weighting.

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

-- | Unified function to perform both the postorder and preorder passes (as relevant)
-- Takes in an overall weight, a vector of metadata, and a tree.
-- Returns a tree with values assigned.
-- This actually calls the postorder and preorder optimizations.
allOptimization :: (TreeConstraint' t n s, Metadata m s) => Double -> Vector m -> t -> t
allOptimization weighting meta inTree = secondPass
    where
        firstPass  = treeOptimizePostorder weighting inTree meta
        secondPass = treeOptimizePreorder firstPass meta

-- | Optimization postorder wrapper to perform relevant algorithm at all nodes
-- Takes in an overall weight, a tree, and a vector of metadata
-- Returns a tree with values assigned
-- Correctly handles roots, leaves, and nodes with only one child
treeOptimizePostorder :: (TreeConstraint' t n s, Metadata m s) => Double -> t -> Vector m -> t
treeOptimizePostorder weighting tree meta = tree `update` (rootNode : nonRootNodes)
  where
    -- We recursively decorate all nodes in the tree, then return the updated tree
    (rootNode, nonRootNodes) = treeInternalPostorderTraversal weighting (root tree) tree meta

-- | Internal postorder optimization pass
-- takes in a weight, a current node, a tree, and a vector of metadata
-- returns a list of nodes that have had changes applied to them
-- By using this node accumulation scheme, we save some complexity over simply always dealing with a tree
treeInternalPostorderTraversal :: (TreeConstraint' t n s, Metadata m s) => Double -> n -> t -> Vector m -> (n, [n])
treeInternalPostorderTraversal weighting node tree meta = (decoratedSelf, decoratedSubtree)
  where
      -- After applying the traversal logic to the subtree of this node
      -- we count the mutated children of this node to determine how to decorate this node.
      --
      -- If the node has two (or more) children, we apply direct optimization and decorate this node with
      -- the "gapped" and "ungapped" results of the direct optimization coparison of the two children.
      --
      -- If the node has a single child, we decorate this node with the exact values of the child node.
      --
      -- If the node has no children, we decorate the leaf node with cost values of zer.
      decoratedSelf =
        case children' of
          left:right:_ -> nodeOptimizePostorder weighting node left right meta
          child:_      -> decorateInternal child node
          _            -> decorateLeaf node

      -- We apply the traversal logic to the nodes children, returning back [(n,[n])]
      -- where the first componet ofeach tuple is a mutated child of this node
      -- and where the second component of the tuple is the list of mutated nodes in the mutated child's subtree.
      recursiveResult            = (\x -> treeInternalPostorderTraversal weighting x tree meta) <$> children node tree

      -- We extract the /only/ mutated children nodes from the resursive result.
      children'                  = fst <$> recursiveResult

      -- We combine the mutated subtrees with the mutated children to create a new mutated subtree for this node.
      decoratedSubtree           = children' <> concatMap snd recursiveResult

      -- Logic for decorating a leaf node
      decorateLeaf     leafNode  = setTotalCost 0.0 $ setLocalCost 0.0 leafNode

      -- Logic for decorating a node with exactly one child.
      -- These nodes are malformed in most (all?) tree topologies.
      decorateInternal otherNode = -- setTemporary   (getTemporary        otherNode)
                                   setPreliminaryGapped   (getPreliminaryGapped   otherNode)
                                 . setPreliminaryUngapped (getPreliminaryUngapped otherNode)
                                 . setTotalCost           (getTotalCost           otherNode)
                                 . setLocalCost           (getLocalCost           otherNode)

-- | Wrapper function to perform optimization on a node during the postorder pass
-- Essentially map decision function that selects and performs the correct optimization over the sequence of characters.
-- Takes in an overall weight, a current node, the left child, the right child, and a vector of metadata
-- Outputs a node with the correct sequences and costs assigned.
nodeOptimizePostorder :: (NodeConstraint' n s, Metadata m s) => Double -> n -> n -> n -> Vector m -> n
nodeOptimizePostorder weighting curNode lNode rNode meta = summedTotalCost `setTotalCost` res
    where
        summedTotalCost = sum [getLocalCost res, getTotalCost lNode, getTotalCost rNode]
        res             = ifoldr chooseOptimization curNode meta

        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization curPos metadataStructure setNode
            -- TODO: Compiler error maybe below with comment structures and 'lets'
            | getIgnored metadataStructure = setNode
            | getType metadataStructure == Fitch =
                let (assign, temp, local) = preorderFitchBit curWeight (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) metadataStructure
                in -- addTemporary temp
                   addLocalCost (local * curWeight * weighting)
                 . addAlign assign
                 . addPreliminary assign
                 $ setNode
            | getType metadataStructure == DirectOptimization =
                let (ungapped, cost, gapped, _, _) = naiveDO (getForAlign lNode ! curPos) (getForAlign rNode ! curPos) $ getCosts metadataStructure
                in addLocalCost (cost * curWeight * weighting)
                 . addAlign gapped
                 . addPreliminary ungapped
                 $ setNode
            | otherwise = error "Unrecognized optimization type"
            where curWeight = getWeight metadataStructure

        addPreliminary addVal node = addToField setPreliminaryUngapped getPreliminaryUngapped addVal node
        addAlign       addVal node = addToField setPreliminaryGapped   getPreliminaryGapped   addVal node
--        addTemporary   addVal node = addToField setTemporary   getTemporary        addVal node
--        addTotalCost   addVal node = setTotalCost (addVal + getTotalCost node) node
        addLocalCost   addVal node = setLocalCost (addVal + getLocalCost node) node

-- | Wrapper for the preorder
-- Takes in a tree and a vector of metadata,
-- returns a tree with relevant nodes assigned.
-- This wrapper allows us to deal correctly with root passing to preorder algorithms
treeOptimizePreorder :: (TreeConstraint' t n s, Metadata m s) => t -> Vector m -> t
treeOptimizePreorder tree meta = tree `update` treeInternalPreorderTraversal (root tree) tree meta

-- | Internal preorder pass that does the main recursion
-- Takes in a current node, the tree, and a vector of metadata;
-- returns a list of nodes that have been updated.
-- As in the postorder, this method saves on some time complexity.
treeInternalPreorderTraversal :: (TreeConstraint' t n s, Metadata m s) => n -> t -> Vector m -> [n]
treeInternalPreorderTraversal node tree meta  = 
  case children' of
      left:right:_ -> nodeOptimizePreorder node left right (parent node tree) meta : result
      _            -> result
  where
      children' = children node tree
      result    = concatMap (\x -> treeInternalPreorderTraversal x tree meta) children'

-- | Wrapper function to perform optimization on a node during the preorder pass.
-- As in the postorder, it selects an optimization for each character, then groups the optimized characters together and assigns them to the node.
-- Takes in a current node, left child, right child, parent node, and vector of metadata,
-- returns a node with everything assigned.
nodeOptimizePreorder :: (NodeConstraint' n s, Metadata m s) => n -> n -> n -> Maybe n -> Vector m -> n
nodeOptimizePreorder curNode lNode rNode pNode meta
    | isNothing pNode = curNode --error "No parent node on preorder traversal"
    | otherwise       = ifoldr chooseOptimization curNode meta
    where
        --chooseOptimization :: (NodeConstraint' n s, Metadata m s) => Int -> m -> n -> n
        chooseOptimization i metadataStructure setNode
            | getType metadataStructure == Fitch =
                let finalAssign = postorderFitchBit (getForAlign curNode ! i) (getForAlign lNode ! i) (getForAlign rNode ! i) (getForAlign (fromJust pNode) ! i) {- (getTemporary curNode ! i) -} (constructDynamic []) metadataStructure
                in addToField setFinal getFinal finalAssign setNode
            | getType metadataStructure == DirectOptimization =  --TODO: do we grab the gapped or not?
                let (final, _, finalAligned, _, _) = naiveDO (getForAlign curNode ! i) (getForAlign (fromJust pNode) ! i) $ getCosts metadataStructure
                in addToField setFinal       getFinal       final
                 . addToField setFinalGapped getFinalGapped finalAligned
                 $ setNode
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
    | null (getPreliminaryGapped node) && null (getPreliminaryUngapped node) = getEncoded node
    | null $ getPreliminaryGapped node                                       = getPreliminaryUngapped node
    | otherwise                                                              = getPreliminaryGapped node

-- | addToField takes in a setter fn, a getter fn, a value and a node.
-- It then gets the related value from the node, adds to it the passed value,
-- and sets that value on the node. It returns a new node with the newly computed value set.
addToField :: NodeConstraint' n s => (Vector s -> n -> n) -> (n -> Vector s) -> s -> n -> n
addToField setter getter val node = setter (pure val <> getter node) node
