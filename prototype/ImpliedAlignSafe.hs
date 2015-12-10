

module ImpliedAlignSafe (implyMain, subtreeWrap, mergeSubtree) where

import Component
import Parsimony (naiveDO)
import ReadFiles
import Data.Matrix (Matrix, zero, setElem, elementwise, getRow, nrows, ncols)
import qualified Data.Vector as V (length, (!), findIndex, cons, ifoldr, (++), singleton, imap, (//), empty, (!?), Vector)
import Data.Maybe
import CharacterData (BaseChar)
import Debug.Trace
import Control.Monad
import Data.Keyed ((!?))
import Data.Functor
import Test.Tasty.QuickCheck

type Subtrees = Matrix Int
data Side = LeftChild | RightChild | Parent  deriving (Eq, Show, Read)
type AlignOut = (BaseChar, Float, BaseChar, BaseChar, BaseChar)

-- | implyMain is a the function that performs an implied alignment for a tree starting at the root
implyMain :: CharInfo -> PhyloComponent -> PhyloComponent
--implyMain info tree | trace ("implyMain " ++ show tree) False = undefined
implyMain info tree = case root of
    Nothing -> error "Tree has no root or root reference is outside tree"
    Just r ->  trace ("subtrees " ++ show subMat)
                iaMainPreorder tree tree subMat (Just r) info

    where 
        root = join $ (tree V.!?) <$> (V.findIndex (\node -> isRoot node) tree)
        subMat = (zero (V.length tree) (V.length tree))

-- | Wrapper for getSubtrees
subtreeWrap :: PhyloComponent -> Subtrees
subtreeWrap tree = fst $ getSubtrees tree root (zero (V.length tree) (V.length tree))
    where 
        root = V.findIndex (\node -> isRoot node) tree

-- | List the subtrees at each node to use in the preorder traversal
getSubtrees :: PhyloComponent -> Maybe NodeCode -> Subtrees -> (Subtrees, [Int])
getSubtrees inTree curCode initStructure | trace ("getSubtrees " ++ show curCode) False = undefined
getSubtrees inTree curCode initStructure = case node of 
        Nothing ->  (initStructure, [])
        Just n ->   if isTerminal n then (initStructure, [fromJust curCode])
                    else    let
                                left = getSubtrees inTree ((children n) !? 0) initStructure
                                right = trace ("got left " ++ show left)
                                            getSubtrees inTree ((children n) !? 1) initStructure
                                totalStruc = trace ("got right " ++ show right)
                                                (fst left) `sumMat` (fst right)
                                totalPos = trace ("positions " ++ show totalStruc)
                                            (snd left) ++ (snd right)
                            in trace ("totalPos " ++ show totalPos)
                                    (accum (totalStruc, totalPos) (fromJust curCode), (fromJust curCode) : totalPos)

        where
            node = join $ (V.!?) inTree <$> curCode
            sumMat = elementwise (+)

            accum :: (Subtrees, [Int]) -> Int -> Subtrees
            accum (downStruc, downPos) code | trace ("accum subtrees " ++ show downPos ++ show code) False = undefined
            accum (downStruc, downPos) code = foldr (\pos acc -> setElemSafe 1 (code, pos) acc) downStruc downPos

            setElemSafe :: Int -> (Int, Int) -> Subtrees -> Subtrees
            setElemSafe value (row, col) matrix | trace ("set elem " ++ show (row, col)) False = undefined
            setElemSafe value (row, col) matrix
                | row >= nrows matrix || col >= ncols matrix || row < 0 || col < 0 = error "Attempt to set a matrix element outside"
                | otherwise = setElem value (row, col) matrix


-- Starts at the given node
iaMainPreorder :: PhyloComponent -> PhyloComponent -> Subtrees -> Maybe PhyloNode -> CharInfo -> PhyloComponent
--iaMainPreorder fullTree subTree subMat node info | trace ("preorder with subtree " ++ show subTree) False = undefined
iaMainPreorder fullTree subTree subMat inNode info = case inNode of 
    Nothing -> subTree
    Just node ->    let outTree | leftCheck && rightCheck = 
                                    let 
                                        alignedLeft = naiveDO (preliminaryGapped node) (alignLeft node) info
                                        updatedTree = changeAlignTree fullTree node LeftChild alignedLeft
                                        secondNode = safeGrab updatedTree node
                                        alignedRight = naiveDO (preliminaryGapped secondNode) (alignRight secondNode) info
                                        finalUpdate = changeAlignTree updatedTree secondNode RightChild alignedRight
                                        (leftEval, rightEval) = recurse finalUpdate subMat leftI rightI
                                        newNode = safeGrab finalUpdate node
                                    in if (length $ preliminaryGapped newNode) > (length $ preliminaryGapped node)
                                            then (implyMain info) $ iaPostorder finalUpdate node info
                                            else  mergeSubtree leftEval rightEval newNode
                                | leftCheck = 
                                    let 
                                        alignedLeft = naiveDO (preliminaryGapped node) (alignLeft node) info
                                        updatedTree = changeAlignTree fullTree node LeftChild alignedLeft
                                        (leftEval, rightEval) = recurse updatedTree subMat leftI rightI
                                        newNode = safeGrab updatedTree node
                                    in if (length $ preliminaryGapped newNode) > (length $ preliminaryGapped node)
                                            then (implyMain info) $ iaPostorder updatedTree node info
                                            else  mergeSubtree leftEval rightEval newNode
                                | rightCheck = 
                                    let 
                                        alignedRight = naiveDO (preliminaryGapped node) (alignRight node) info
                                        finalUpdate = changeAlignTree fullTree node RightChild alignedRight
                                        (leftEval, rightEval) = recurse finalUpdate subMat leftI rightI
                                        newNode = safeGrab finalUpdate node
                                    in if (length $ preliminaryGapped newNode) > (length $ preliminaryGapped node)
                                            then (implyMain info) $ iaPostorder finalUpdate node info
                                            else  mergeSubtree leftEval rightEval newNode
                                | otherwise = 
                                    let (leftEval, rightEval) = recurse fullTree subMat leftI rightI
                                    in mergeSubtree leftEval rightEval node
                    in outTree
        where 
            grabAlign :: AlignOut -> BaseChar
            grabAlign (a, _, _, _, _) = a

            leftI = (children node) !? 0
            rightI = (children node) !? 1
            leftCheck = checkAlign (Just node) (join $ (fullTree V.!?) <$> leftI)
            rightCheck = checkAlign (Just node) (join $ (fullTree V.!?) <$> rightI)

            recurse :: PhyloComponent -> Subtrees -> Maybe Int -> Maybe Int -> (PhyloComponent, PhyloComponent)
            recurse updatedTree subMat leftI rightI | trace ("recurse " ++ show leftI ++ show rightI) False = undefined
            recurse updatedTree subMat leftI rightI = 
                let 
                    leftTree = grabSubtree updatedTree leftI subMat
                    rightTree = grabSubtree updatedTree rightI subMat
                    leftEval = iaMainPreorder updatedTree leftTree subMat (join $ (updatedTree V.!?) <$> leftI) info
                    rightEval = iaMainPreorder updatedTree rightTree subMat (join $ (updatedTree V.!?) <$> rightI) info
                in (leftEval, rightEval)

            safeGrab :: PhyloComponent -> PhyloNode -> PhyloNode
            safeGrab tree node = case tree V.!? (code node) of
                                    Nothing -> error "Problem in updating node"
                                    Just n -> n

            checkAlign :: Maybe PhyloNode -> Maybe PhyloNode -> Bool
            checkAlign n1 n2 
                | isNothing n1 || isNothing n2 = False
                | (length $ preliminaryGapped $ fromJust n1) /= (length $ preliminaryGapped $ fromJust n1) = True
                | otherwise = False
            
-- | Postorder traversal up the tree to resolve parents and ancestors
iaPostorder :: PhyloComponent -> PhyloNode -> CharInfo -> PhyloComponent
--iaPostorder tree curNode info | trace "postorder" False = undefined
iaPostorder tree curNode info = case mparent of
    Nothing -> tree
    Just parent -> let outTree  | (V.length $ preliminaryGapped parent) /= (V.length $ preliminaryGapped curNode) = 
                                    let 
                                        aligned = naiveDO (preliminaryGapped curNode) (preliminaryGapped parent) info
                                        updatedTree = changeAlignTree tree curNode Parent aligned
                                    in iaPostorder updatedTree parent info
                                | otherwise = iaPostorder tree parent info
                    in outTree
    where
        mparent = join $ (tree V.!?) <$> ((parents curNode) !? 0)


-- | Function to grab a subtree given a matrix and a row to use
grabSubtree :: PhyloComponent -> Maybe Int -> Subtrees -> PhyloComponent
grabSubtree fullTree mrow matrix | trace ("grabSubtree " ++ show matrix) False = undefined
grabSubtree fullTree mrow matrix = case mrow of 
    Nothing -> trace "nothing grab" $ V.empty
    Just row -> 
                let 
                    grabRow = getRow row matrix
                    nodes = trace ("got row " ++ show grabRow)
                                V.ifoldr (\i on acc -> acc V.++ foldSubtree i on fullTree) V.empty grabRow
                in nodes

    where
        foldSubtree :: Int -> Int -> PhyloComponent -> V.Vector PhyloNode
        foldSubtree pos element tree 
            | element == 1 && (isJust node) = V.singleton (fromJust node)
            | otherwise = V.empty
            where node = tree V.!? pos

-- | Function to merge a subtree from left and right subtrees as well as current node
mergeSubtree :: PhyloComponent -> PhyloComponent -> PhyloNode -> PhyloComponent
--mergeSubtree leftTree rightTree node | trace ("mergeSubtree " ++ show leftTree ++ show rightTree) False = undefined
mergeSubtree leftTree rightTree node = 
    let 
        leftIndexed = reindexTree leftTree 0 
        rightIndexed = reindexTree rightTree (V.length leftIndexed)
        totalLen = (V.length leftIndexed) + (V.length rightIndexed)
        nodeIndexed = case totalLen of 
                        0 -> node {code = 0}
                        _ -> node {code = totalLen - 1}
    in leftIndexed V.++ rightIndexed V.++ (V.singleton nodeIndexed)

    where
        reindexTree :: PhyloComponent -> Int -> PhyloComponent
        reindexTree tree startIndex = V.imap (\i node -> node {code = i + startIndex}) tree

-- | Function to make necessary changes to tree
changeAlignTree :: PhyloComponent -> PhyloNode -> Side -> AlignOut -> PhyloComponent
--changeAlignTree inTree inNode side (_, _, _, left, right) | trace ("update alignments on " ++ show inNode) False = undefined
changeAlignTree inTree inNode side (_, _, _, left, right) = case grabMine of
    Nothing -> inTree
    Just child -> 
        let 
            newNode = inNode {preliminaryGapped = left}
            newChild = child {preliminaryGapped = right}
        in inTree V.// [(code inNode, newNode), (code child, newChild)]

    where
        grabMine = safeGrab inTree side inNode
        safeGrab :: PhyloComponent -> Side -> PhyloNode -> Maybe PhyloNode
        safeGrab tree side node
            | side == LeftChild = getLeft
            | side == RightChild = getRight
            | side == Parent = getParent
            | otherwise = Nothing
                where
                    getLeft = join $ (tree V.!?) <$> ((children node) !? 0)
                    getRight = join $ (tree V.!?) <$> ((children node) !? 1)
                    getParent = join $ (tree V.!?) <$> ((parents node) !? 0)

