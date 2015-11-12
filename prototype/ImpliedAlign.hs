

module ImpliedAlign (implyMain) where

import Component
import Parsimony (ukkonenDO)
import ReadFiles
import Data.Matrix (Matrix, zero, setElem, elementwise, getRow)
import qualified Data.Vector as V (length, (!), findIndex, cons, ifoldr, empty, (++), singleton, imap, (//))
import Data.Maybe
import CharacterData (BaseChar)

type Subtrees = Matrix Integer
data Side = LeftChild | RightChild | Parent deriving (Eq, Show, Read)
type AlignOut = (BaseChar, Float, BaseChar, BaseChar, BaseChar)

-- | implyMain is a the function that performs an implied alignment for a tree starting at the root
implyMain :: CharInfo -> PhyloComponent -> PhyloComponent
implyMain info tree = iaMainPreorder tree tree subMat (tree V.! root) info
    where 
        root = fromJust $ V.findIndex (\node -> isRoot node) tree
        (subMat, _) = getSubtrees tree root (zero (V.length tree) (V.length tree))

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
            node = inTree V.! curCode
            sumMat = elementwise (+)

-- | iaMainDown is the main downpass of an implied alignment
-- Starts at the given node
iaMainPreorder :: PhyloComponent -> PhyloComponent -> Subtrees -> PhyloNode -> CharInfo -> PhyloComponent
iaMainPreorder fullTree subTree subMat node info
     | (length $ children node) > 2 = error "Implied alignment only implemented for binary trees"
     | null $ preliminaryStates node = error "Cannot perform implied alignment without preliminary states"
     | isTerminal node = subTree
     | (length $ preliminaryGapped node) /= (length $ alignLeft node) && (not $ null $ alignLeft node) = 
        let 
            aligned = ukkonenDO (preliminaryGapped node) (alignLeft node) info
            updatedTree = changeAlignTree fullTree node LeftChild aligned
        in  
            if (length $ grabAlign aligned) > (length $ preliminaryGapped node)
                then (implyMain info) $ iaPostorder fullTree node info
                else  mergeSubtree leftEval rightEval (updatedTree V.! (code node))
     | (length $ preliminaryGapped node) /= (length $ alignRight node) && (not $ null $ alignRight node) = 
        let 
            aligned = ukkonenDO (preliminaryGapped node) (alignRight node) info
            updatedTree = changeAlignTree fullTree node RightChild aligned
        in  
            if (length $ grabAlign aligned) > (length $ preliminaryGapped node)
                then (implyMain info) $ iaPostorder updatedTree node info
                else  mergeSubtree leftEval rightEval (updatedTree V.! (code node))
     | otherwise = mergeSubtree leftEval rightEval node
        where 
            grabAlign :: AlignOut -> BaseChar
            grabAlign (a, _, _, _, _) = a

            leftI = head $ children node
            rightI = (children node) !! 1
            leftTree = grabSubtree fullTree leftI subMat
            rightTree = grabSubtree fullTree rightI subMat
            leftEval = iaMainPreorder fullTree leftTree subMat (fullTree V.! leftI) info
            rightEval = iaMainPreorder fullTree rightTree subMat (fullTree V.! rightI) info
            
-- | Postorder traversal up the tree to resolve parents and ancestors
iaPostorder :: PhyloComponent -> PhyloNode -> CharInfo -> PhyloComponent
iaPostorder tree curNode info 
    | (length $ parents curNode) > 1 = error "Cannot perform implied alignment on networks"
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
changeAlignTree inTree inNode side (_, _, _, left, right)
    | side == LeftChild = 
        let 
            newNode = inNode {preliminaryGapped = left, alignLeft = right}
            newChild = leftChild {preliminaryGapped = right}
        in inTree V.// [(code inNode, newNode), (code leftChild, newChild)]
    | side == RightChild = 
        let 
            newNode = inNode {preliminaryGapped = left, alignRight = right}
            newChild = rightChild {preliminaryGapped = right}
        in inTree V.// [(code inNode, newNode), (code rightChild, newChild)]
    | side == Parent = 
        let 
            newNode = inNode {preliminaryGapped = left}
            newParent   | (head $ children parent) == (code inNode) = parent {preliminaryGapped = right, alignLeft = left}
                        | otherwise = parent {preliminaryGapped = right, alignRight = left}
        in inTree V.// [(code inNode, newNode), (code parent, newParent)]
    | otherwise = error "Problem with alignment setting"
        where
            leftChild = (V.!) inTree $ head $ children inNode
            rightChild = (V.!) inTree $ (!!) (parents inNode) 1
            parent = (V.!) inTree $ head $ parents inNode

