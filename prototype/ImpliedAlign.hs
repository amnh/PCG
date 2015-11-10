

module ImpliedAlign (implyMain) where

import Component
import Parsimony (ukonnenDO)
import Data.Map.Strict (Map) 
import ReadFiles
import Data.Matrix (Matrix, zero, setElem, elementwise)
import Data.Vector ((!), findIndex, length)
import Data.Maybe

type Subtrees = Matrix Int

-- | implyMain is a the function that performs an implied alignment for a tree starting at the root
implyMain :: CharInfo -> PhyloComponent -> PhyloComponent
implyMain info tree = iaMainPreorder tree subMat (tree ! root) info
    where 
        root = fromJust $ findIndex (\node -> isRoot node) tree
        (subMat, _) = getSubtrees tree root (zero (length tree) (length tree))

-- | List the subtrees at each node to use in the preorder traversal
getSubtrees :: PhyloComponent -> NodeCode -> Subtrees -> (Subtrees, [Int])
getSubtrees inTree curCode initStructure
    | isTerminal node = (initStructure, [curCode])
    | (length $ children node) > 2 = error "Can only perform implied alignment on binary trees"
    | (length $ children node) == 1 = 
        let
            child = head $ children node
            (downStruc, downPos) = getSubtrees inTree child initStructure
            setMat = foldr (\pos acc -> setElem 1 (curCode, pos) acc) downStruc downPos
        in (setMat, curCode : downPos)
    | otherwise = 
        let
            (leftStruc, leftPos) = getSubtrees inTree (head $ children node) initStructure
            (rightStruc, rightPos) = getSubtrees inTree (head $ tail $ children node) initStructure
            totalStruc = leftStruc `sumMat` rightStruc
            totalPos = leftPos ++ rightPos
            setMat = foldr (\pos acc -> setElem 1 (curCode, pos) acc) totalStruc totalPos
        in (setMat, curCode : totalPos)
        where
            node = inTree ! curCode
            sumMat = elementwise (+)

-- | iaMainDown is the main downpass of an implied alignment
-- Starts at the given node
iaMainPreorder :: PhyloComponent -> PhyloComponent -> Subtrees -> PhyloNode -> CharInfo -> PhyloComponent
iaMainPreorder fullTree subTree subMat node info
     | length $ children node > 2 = error "Implied alignment only implemented for binary trees"
     | null $ preliminaryStates node = error "Cannot perform implied alignment without preliminary states"
     | isTerminal node = inTree
     | (length . preliminaryGapped node) /= (length . alignLeft node) && (not $ null alignLeft node) = 
        let 
            aligned = ukonnenDO (preliminaryGapped node) alignLeft info
            
        if (length aligned) > (length . preliminaryGapped node)
            then (implyMain info) . iaPostorder fullTree node info
            else  
     | (length . preliminaryGapped node) /= (length . alignRight node) && (not $ null alignRight node) = 

     | length $ children node == 1 = 
        let

        in 
     | otherwise = 
        let

        in 

-- | Postorder traversal up the tree to resolve parents and ancestors
iaPostorder :: PhyloComponent -> PhyloNode -> CharInfo -> PhyloComponent

-- | Function to re-index a tree from the starting point
-- allows for splitting and combination of trees without code issues
combineTrees :: PhyloComponent -> Int -> PhyloComponent
combineTrees tree startIndex = undefined

-- | Simple function to set the aligned data for a node
setAlign :: PhyloNode -> [BaseChar] -> PhyloNode
setAlign inNode aligned
    | null aligned = inNode
    | length aligned == 1 = inNode { preliminaryGapped = (aligned !! 0)}
    | length aligned == 2 = inNode { preliminaryGapped = (aligned !! 0), alignLeft = (aligned !! 1)}
    | otherwise = inNode { preliminaryGapped = (aligned !! 0), alignLeft = (aligned !! 1), alignRight = (aligned !! 2)}
