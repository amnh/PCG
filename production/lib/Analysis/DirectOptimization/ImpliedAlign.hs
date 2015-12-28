{-# LANGUAGE ConstraintKinds #-}

module Analysis.DirectOptimization.ImpliedAlign (implyMain, getSubtrees) where

import Bio.Phylogeny.Network
import Bio.Phylogeny.Tree.CharacterAware
import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.Binary
import Bio.Sequence.Coded
import Bio.Phylogeny.Tree.Referential

import Analysis.DirectOptimization.Utilities
import Analysis.DirectOptimization.Naive

import Prelude hiding (zipWith)
import Data.Matrix (Matrix, zero, elementwise, nrows, ncols, getRow)
import Data.Bits
import Data.Maybe
import Data.Vector (zipWith)

import Control.Monad (join)
import Control.Applicative (liftA2)
import Data.Monoid

-- | implyMain performs an implied alignment on a tree
implyMain :: TreeConstraint t n s b => t -> t
implyMain inTree = iaMainPreorder inTree inTree subMat (Just $ root inTree)
    where
        subMat = getSubtrees inTree

-- | Main implied alignment function to save info to a tree
iaMainPreorder :: TreeConstraint t n s b => t -> t -> Subtrees -> Maybe n -> t
iaMainPreorder fullTree subTree subtrees inNode 
    | isNothing inNode = subTree
    | leftCheck && rightCheck = 
        let
            (leftAlign, rightAlign, curAlign, isLonger) = naiveDOThree (fromJust left) (fromJust right) (fromJust inNode)
            updatedTree = fullTree `update` [leftAlign, rightAlign, curAlign]
            in recursiveIA updatedTree subtrees left right inNode isLonger
    | leftCheck = 
        let
            (leftAlign, curAlign, isLonger) = naiveDOTwo (fromJust left) (fromJust inNode)
            updatedTree = fullTree `update` [leftAlign, curAlign]
            in recursiveIA updatedTree subtrees left right inNode isLonger
    | rightCheck = 
        let
            (rightAlign, curAlign, isLonger) = naiveDOTwo (fromJust right) (fromJust inNode)
            updatedTree = fullTree `update` [rightAlign, curAlign]
        in recursiveIA updatedTree subtrees left right inNode isLonger
    | otherwise = recursiveIA fullTree subtrees left right inNode False

        where
            left = join $ (flip leftChild) subTree <$> inNode
            right = join $ (flip rightChild) subTree <$> inNode
            leftCheck = checkAlign left inNode
            rightCheck = checkAlign right inNode

            -- | Common recursive call for an implied alignment
            recursiveIA :: TreeConstraint t n s b => t -> Subtrees -> Maybe n -> Maybe n -> Maybe n -> Bool -> t
            recursiveIA updatedTree subtrees leftNode rightNode inNode isLonger 
                | isLonger = implyMain $ iaPostorder updatedTree inNode
                | otherwise = merged
                    where
                        leftTree = grabSubtree updatedTree (join $ (flip code) updatedTree <$> leftNode) subtrees
                        leftEval = iaMainPreorder updatedTree leftTree subtrees leftNode
                        rightTree = grabSubtree updatedTree (join $ (flip code) updatedTree <$> rightNode) subtrees
                        rightEval = iaMainPreorder updatedTree rightTree subtrees rightNode
                        merged = mergeSubtrees leftEval rightEval inNode

            -- | Function to merge two subtrees under their parent node
            mergeSubtrees :: TreeConstraint t n s b => t -> t -> Maybe n -> t
            mergeSubtrees left right node 
                | isNothing node = mempty
                | otherwise = (left <> right) `addNode` fromJust node

-- | Postorder traversal of the implied alignment, started at the given node on the given tree
iaPostorder :: TreeConstraint t n s b => t -> Maybe n -> t
iaPostorder inTree curNode 
    | isNothing curNode || isNothing curParent = inTree
    | isAligned = iaPostorder inTree curParent
    | otherwise = 
        let
           (parentAlign, curAlign, isLonger) = naiveDOTwo (fromJust curNode) (fromJust curParent) 
           updatedTree = inTree `update` [parentAlign, curAlign]
        in iaPostorder updatedTree curParent
        where
            curParent = join $ (flip parent) inTree <$> curNode
            isAligned = checkAlign curParent curNode

-- | Helper function to check whether two nodes have the same length sequences
checkAlign :: NodeConstraint n s b => Maybe n -> Maybe n -> Bool
checkAlign childNode parentNode 
    | isNothing childNode || isNothing parentNode = False
    | otherwise = 
        let checkLen = zipWith (\align preAlign -> if numChars align > numChars preAlign then True else False) (preliminaryAlign $ fromJust childNode) (getForAlign $ fromJust parentNode)
        in or checkLen

