

module ImpliedAlign (implyMain, subtreeWrap, mergeSubtree) where

import Component
import Parsimony (ukkonenDO)
import ReadFiles
import Data.Matrix (Matrix, zero, setElem, elementwise, getRow)
import qualified Data.Vector as V (length, (!), findIndex, cons, ifoldr, (++), singleton, imap, (//), empty)
import Data.Maybe
import CharacterData (BaseChar)
import Debug.Trace

type Subtrees = Matrix Int
data Side = LeftChild | RightChild | Parent  deriving (Eq, Show, Read)
type AlignOut = (BaseChar, Float, BaseChar, BaseChar, BaseChar)

-- | implyMain is a the function that performs an implied alignment for a tree starting at the root
implyMain :: CharInfo -> PhyloComponent -> PhyloComponent
implyMain info tree 
    | isNothing root = error "Tree has no root"
    | otherwise = iaMainPreorder tree tree subMat (tree V.! (fromJust root)) info
    where 
        root = V.findIndex (\node -> isRoot node) tree
        subMat = subtreeWrap tree

-- | Wrapper for getSubtrees
subtreeWrap :: PhyloComponent -> Subtrees
subtreeWrap tree 
    | isNothing root = error "Tree has no root"
    | otherwise = fst $ getSubtrees tree (fromJust root) (zero (V.length tree) (V.length tree))

        where root = V.findIndex (\node -> isRoot node) tree

-- | List the subtrees at each node to use in the preorder traversal
getSubtrees :: PhyloComponent -> NodeCode -> Subtrees -> (Subtrees, [Int])
--getSubtrees inTree curCode initStructure | trace "getSubtrees" False = undefined
getSubtrees inTree curCode initStructure
    | isTerminal node || (null $ children node) = (initStructure, [curCode])
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
            (rightStruc, rightPos) = getSubtrees inTree ((children node) !! 1) initStructure
            totalStruc = leftStruc `sumMat` rightStruc
            totalPos = leftPos ++ rightPos
            setMat = foldr (\pos acc -> setElem 1 (curCode, pos) acc) totalStruc totalPos
        in (setMat, curCode : totalPos)
        where
            node = inTree V.! curCode
            sumMat = elementwise (+)

-- | iaMainDown is the main downpass of an implied alignment
-- Starts at the given node
iaMainPreorder :: PhyloComponent -> PhyloComponent -> Subtrees -> PhyloNode -> CharInfo -> PhyloComponent
iaMainPreorder fullTree subTree subMat node info | trace ("preorder ") False = undefined
iaMainPreorder fullTree subTree subMat node info
     | (length $ children node) > 2 = error "Implied alignment only implemented for binary trees"
     | null $ preliminaryGapped node = subTree
     | isTerminal node || null (children node) = subTree
     | leftCheck && rightCheck && (not $ null $ alignLeft node) && (not $ null $ alignRight node) = trace ("both case " ++ show node) $
        let 
            alignedLeft = ukkonenDO (preliminaryGapped node) (alignLeft node) info
            updatedTree = trace ("set alignment left " ++ show alignedLeft) $
                            changeAlignTree fullTree node LeftChild alignedLeft
            secondNode = updatedTree V.! (code node)
            alignedRight = ukkonenDO (preliminaryGapped secondNode) (alignRight secondNode) info
            finalUpdate = trace ("set alignment right " ++ show alignedRight) $
                        changeAlignTree updatedTree secondNode RightChild alignedRight
            newSub = grabSubtree finalUpdate (code node) subMat
            --finalUpdate = iaMainPreorder updated2 newSub subMat node info
            leftTree = grabSubtree finalUpdate leftI subMat
            rightTree = grabSubtree finalUpdate rightI subMat
            leftEval = iaMainPreorder finalUpdate leftTree subMat (finalUpdate V.! leftI) info
            rightEval = iaMainPreorder finalUpdate rightTree subMat (finalUpdate V.! rightI) info
            newNode = finalUpdate V.! (code node)
        in  
            if (length $ preliminaryGapped newNode) > (length $ preliminaryGapped node)
                then (implyMain info) $ iaPostorder finalUpdate node info
                else  mergeSubtree leftEval rightEval newNode

     | leftCheck && (not $ null $ alignLeft node) = 
        let 
            aligned = ukkonenDO (preliminaryGapped node) (alignLeft node) info
            updatedTree = changeAlignTree fullTree node LeftChild aligned
            leftTree = grabSubtree updatedTree leftI subMat
            rightTree = grabSubtree updatedTree rightI subMat
            leftEval = iaMainPreorder updatedTree leftTree subMat (updatedTree V.! leftI) info
            rightEval = iaMainPreorder updatedTree rightTree subMat (updatedTree V.! rightI) info
            newNode = updatedTree V.! (code node)
        in  
            if (length $ grabAlign aligned) > (length $ preliminaryGapped node)
                then (implyMain info) $ iaPostorder updatedTree node info
                else  mergeSubtree leftEval rightEval newNode
     | rightCheck && (not $ null $ alignRight node) = 
        let 
            aligned = ukkonenDO (preliminaryGapped node) (alignRight node) info
            updatedTree = changeAlignTree fullTree node RightChild aligned
            leftTree = grabSubtree updatedTree leftI subMat
            rightTree = grabSubtree updatedTree rightI subMat
            leftEval = iaMainPreorder updatedTree leftTree subMat (updatedTree V.! leftI) info
            rightEval = iaMainPreorder updatedTree rightTree subMat (updatedTree V.! rightI) info
            newNode = updatedTree V.! (code node)
        in  
            if (length $ grabAlign aligned) > (length $ preliminaryGapped node)
                then (implyMain info) $ iaPostorder updatedTree node info
                else  mergeSubtree leftEval rightEval newNode
     | otherwise = 
        let
            leftTree = grabSubtree fullTree leftI subMat
            rightTree = grabSubtree fullTree rightI subMat
            leftEval = iaMainPreorder fullTree leftTree subMat (fullTree V.! leftI) info
            rightEval = iaMainPreorder fullTree rightTree subMat (fullTree V.! rightI) info
        in mergeSubtree leftEval rightEval node
        where 
            grabAlign :: AlignOut -> BaseChar
            grabAlign (a, _, _, _, _) = a

            leftI = head $ children node
            rightI = (children node) !! 1
            leftCheck = (length $ preliminaryGapped node) /= (length $ alignLeft node)
            rightCheck = (length $ preliminaryGapped node) /= (length $ alignRight node)
            
-- | Postorder traversal up the tree to resolve parents and ancestors
iaPostorder :: PhyloComponent -> PhyloNode -> CharInfo -> PhyloComponent
iaPostorder tree curNode info | trace "postorder" False = undefined
iaPostorder tree curNode info 
    | (length $ parents curNode) > 1 = error "Cannot perform implied alignment on networks"
    | isRoot curNode || null (parents curNode) = tree
    | (V.length $ preliminaryGapped parent) /= (V.length $ preliminaryGapped curNode) = 
        let 
            aligned = ukkonenDO (preliminaryGapped curNode) (preliminaryGapped parent) info
            updatedTree = changeAlignTree tree curNode Parent aligned
        in iaPostorder updatedTree parent info
    | otherwise = iaPostorder tree parent info
    where
        parent = (V.!) tree . head $ parents curNode


-- | Function to grab a subtree given a matrix and a row to use
grabSubtree :: PhyloComponent -> Int -> Subtrees -> PhyloComponent
grabSubtree fullTree row matrix = 
    let 
        grabRow = getRow row matrix
        nodes = V.ifoldr (\i on acc -> if on == 1 then (fullTree V.! i) `V.cons` acc else acc) V.empty grabRow
    in nodes

-- | Function to merge a subtree from left and right subtrees as well as current node
mergeSubtree :: PhyloComponent -> PhyloComponent -> PhyloNode -> PhyloComponent
mergeSubtree leftTree rightTree node = 
    let 
        leftIndexed = reindexTree leftTree 0 
        rightIndexed = reindexTree rightTree (V.length leftIndexed)
        totalLen = (V.length leftIndexed) + (V.length rightIndexed)
        nodeIndexed = node {code = totalLen - 1}
    in leftIndexed V.++ rightIndexed V.++ (V.singleton nodeIndexed)

    where
        reindexTree :: PhyloComponent -> Int -> PhyloComponent
        reindexTree tree startIndex = V.imap (\i node -> node {code = i + startIndex}) tree

-- | Function to make necessary changes to tree
changeAlignTree :: PhyloComponent -> PhyloNode -> Side -> AlignOut -> PhyloComponent
changeAlignTree inTree inNode side (_, _, _, left, right) | trace ("update alignments on " ++ show inNode) False = undefined
changeAlignTree inTree inNode side (_, _, _, left, right)
    | (side == LeftChild && null (children inNode)) || (side == RightChild && (length (children inNode)) < 2) || (side == Parent && null (parents inNode)) = inTree
    | side == LeftChild = 
        let 
            leftChild = (V.!) inTree $ head $ children inNode
            newNode = inNode {preliminaryGapped = left, alignLeft = right}
            newChild = leftChild {preliminaryGapped = right}
        in inTree V.// [(code inNode, newNode), (code leftChild, newChild)]
    | side == RightChild = 
        let 
            rightChild = (V.!) inTree $ (!!) (parents inNode) 1
            newNode = inNode {preliminaryGapped = left, alignRight = right}
            newChild = rightChild {preliminaryGapped = right}
        in inTree V.// [(code inNode, newNode), (code rightChild, newChild)]
    | side == Parent = 
        let 
            parent = (V.!) inTree $ head $ parents inNode
            newNode = inNode {preliminaryGapped = left}
            newParent   | (head $ children parent) == (code inNode) = parent {preliminaryGapped = right, alignLeft = left}
                        | otherwise = parent {preliminaryGapped = right, alignRight = left}
        in inTree V.// [(code inNode, newNode), (code parent, newParent)]
    | otherwise = error "Problem with alignment setting"
            
           

