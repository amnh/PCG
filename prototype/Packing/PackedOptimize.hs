{- Module for non-additive optimization of a bit packed tree-}

module Packing.PackedOptimize (allOptimization, optimizeForest, getRootCost, costForest) where

-- imports 
import Packing.PackedBuild
import Component
import qualified Data.Vector as V
import qualified Packing.BitPackedNode as BN
import Debug.Trace

-- | Useful higher level data structures of trees, costs, etc
type TreeInfo = (PhyloComponent, PackedTree)
type ExpandTree = (PhyloComponent, PackedTree, PackedTree)
type NewRows = [(Int, BN.BitPackedNode)]
type NewNodes = [(Int, PhyloNode)]
type NodeCost = V.Vector Float

-- | Function to get the cost of an entire forest by map
costForest :: PhyloForest -> (PackedForest, PackedInfo, BN.PackMode) -> Float -> [Float]
costForest forestTrees (packForest, pInfo, pMode) weight = 
    let 
        downOut = zipWith (\dat tree -> optimizationDownPass (tree, dat) pInfo pMode weight) (V.toList packForest) forestTrees
        costs = map (\(tree, _, _) -> V.head $ getRootCost tree) downOut
    in costs

-- | Function to optimize an entire forest by a map
optimizeForest :: PhyloForest -> (PackedForest, PackedInfo, BN.PackMode) -> Float -> [TreeInfo]
optimizeForest forestTrees (packForest, pInfo, pMode) weight = zipWith (\dat tree -> allOptimization (tree, dat) pInfo pMode weight) (V.toList packForest) forestTrees

-- | Unified function to perform both the first and second passes of fitch
allOptimization :: TreeInfo -> PackedInfo -> BN.PackMode -> Float -> TreeInfo
allOptimization input pInfo pMode weight = 
    let 
        downPass = optimizationDownPass input pInfo pMode weight
        upPass = optimizationUpPass downPass pInfo pMode
    in upPass

-- | Optimization down pass warpper for recursion from root
optimizationDownPass :: TreeInfo -> PackedInfo -> BN.PackMode -> Float -> ExpandTree
optimizationDownPass (tree, mat) pInfo pMode weight
    | (length $ children root) > 2 = error "Fitch algorithm only applies to binary trees" -- more than two children means fitch won't work
    | isTerminal root = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let 
            newNode = modifyTotalCost root (V.singleton 0)
            newTree = (V.//) tree [(code root, newNode)]
        in (newTree, mat, mat)
    | (length $ children root) == 1 = -- if there is only one child, continue recursion down and resolve
        let 
            leftChild = head $ children root
            (nodes1, mRows1, fRows1) = internalDownPass (tree, mat) pInfo leftChild pMode weight
            carryBit = snd $ head mRows1 -- with only one child, assignment and cost is simply carried up
            carryFBit = snd $ head fRows1
            carryCost = totalCost $ snd $ head nodes1
            newNodes = (code root, (modifyTotalCost root carryCost)) : nodes1
            allMChanges = (code root, carryBit) : mRows1
            allFChanges = (code root, carryFBit) : fRows1
            combmat = combChanges allMChanges mat
            fcombmat = combChanges allFChanges mat
            combTree = combTreeChanges newNodes tree
        in (combTree, combmat, fcombmat)
    | otherwise = -- if there are two children, do two recursive calls, get node assignment, and then resolve
        let 
            leftChild = head $ children root
            rightChild = head $ tail $ children root
            (nodes1, mRows1, fRows1) = internalDownPass (tree, mat) pInfo leftChild pMode weight
            (nodes2, mRows2, fRows2) = internalDownPass (tree, mat) pInfo rightChild pMode weight
            lbit = snd $ head mRows1
            rbit = snd $ head mRows2
            lcost = totalCost $ snd $ head nodes1
            rcost = totalCost $ snd $ head nodes2
            (mybit, myF, myCost) = downBitOps (lcost, lbit) (rcost, rbit) pInfo pMode weight
            newNode = modifyTotalCost root myCost
            treeUpdates = (code root, newNode) : (nodes1 ++ nodes2)
            allMChanges = (code root, mybit) : (mRows1 ++ mRows2) -- new bits always added to head to pass to bit ops
            allFRows = (code root, myF) : (fRows1 ++ fRows2)
            combmat = combChanges allMChanges mat
            fcombmat = combChanges allFRows mat
            combTree = combTreeChanges treeUpdates tree
        in --trace ("new matrix " ++ show combmat)
            (combTree, combmat, fcombmat)

        where root = head $ [x | x <- (V.toList tree), isRoot x]

-- | Internal down pass that creates new rows without combining, making the algorithm faster
internalDownPass :: TreeInfo -> PackedInfo -> Int -> BN.PackMode -> Float -> (NewNodes, NewRows, NewRows)
--internalDownPass (tree, mat) pInfo myCode pMode | trace ("internal down pass with code " ++ show (mat V.! myCode)) False = undefined
internalDownPass (tree, mat) pInfo myCode pMode weight
    | isTerminal node = --if it's a leaf, just vie cost zero and bounce up
        let newNode = modifyTotalCost node (V.singleton 0)
        in ([(myCode, newNode)], [(myCode, mat V.! myCode)], [(myCode, mat V.! myCode)])
    | (length $ children node) > 2 = error "Fitch algorithm only works for binary trees"
    | (length $ parents node) > 1 = error "Fitch algorithm only works for trees, not networks" -- check for non-binary trees and networks
    | (length $ children node) == 1 = --if only one child, recurse down and carry the assignment
        let 
            leftChild = head $ children node
            (nodes1, mRows1, fRows1) = internalDownPass (tree, mat) pInfo leftChild pMode weight
            carryBit = --trace ("carry bit " ++ show mRows1 ++ " and code " ++ show myCode)
                        snd $ head mRows1
            carryFBit = snd $ head fRows1
            carryCost = totalCost $ snd $ head nodes1
            newNodes = (myCode, (modifyTotalCost node carryCost)) : nodes1
            allMChanges = (myCode, carryBit) : mRows1
            allFChanges = (myCode, carryFBit) : fRows1
        in (newNodes, allMChanges, allFChanges)
    | otherwise = -- if two children, do recursive calls and get node assignment
        let 
            leftChild = --trace ("children " ++ show (children node))
                        head $ children node
            rightChild = head $ tail $ children node
            (nodes1, mRows1, fRows1) = internalDownPass (tree, mat) pInfo leftChild pMode weight
            (nodes2, mRows2, fRows2) = internalDownPass (tree, mat) pInfo rightChild pMode weight
            lbit = snd $ head mRows1
            rbit = snd $ head mRows2
            lcost = totalCost $ snd $ head nodes1
            rcost = totalCost $ snd $ head nodes2
            (mybit, myF, myCost) = downBitOps (lcost, lbit) (rcost, rbit) pInfo pMode weight
            newNode = modifyTotalCost node myCost
            treeUpdates = (myCode, newNode) : (nodes1 ++ nodes2)
            allMChanges = (myCode, mybit) : (mRows1 ++ mRows2) -- new bits always added to head to pass to bit ops
            allFRows = (myCode, myF) : (fRows1 ++ fRows2)
        in (treeUpdates, allMChanges, allFRows)

        where node = tree V.! myCode

-- | Bit operations for the down pass: basically creats a mask for union and intersection areas and then takes them
-- returns the new assignment, the union/intersect mask, and the new total cost
downBitOps :: (NodeCost, BN.BitPackedNode) -> (NodeCost, BN.BitPackedNode) -> PackedInfo -> BN.PackMode -> Float -> (BN.BitPackedNode, BN.BitPackedNode, NodeCost)
--downBitOps (lcost, lbit) (rcost, rbit) pInfo pMode weight | trace ("down bit ops with bit " ++ show (BN.bitSize lbit) ++ " and bit " ++ show (BN.bitSize rbit)) False = undefined
downBitOps (lcost, lbit) (rcost, rbit) pInfo pMode weight =
    let
        notOr = BN.complement $ lbit BN..&. rbit 
        union = lbit BN..|. rbit
        fBit = notOr BN..&. (snd $ masks pInfo)
        rightF = BN.blockShiftAndFold "R" "&" notOr (blockLenMap pInfo) (length $ maxAlphabet pInfo) fBit
        finalF = BN.blockShiftAndFold "L" "|" rightF (blockLenMap pInfo) (length $ maxAlphabet pInfo) rightF
        maskF = --trace ("mask length "++ show (BN.bitSize (fst $ masks pInfo)))
                    (fst $ masks pInfo) BN..&. finalF
        myCost = BN.getNodeCost maskF pMode (blockLenMap pInfo) (length $ maxAlphabet pInfo)
        weightCost = weight * myCost
        newcost = V.singleton $ (V.head lcost) + (V.head rcost) + weightCost
        outbit = (maskF BN..&. union) BN..|. (lbit BN..&. rbit)
    in --trace ("finished bit ops " ++ show outbit)
        (outbit, maskF, newcost)

-- | Combines the changes made to two different matrices given a base matrix
-- assumes the same row isn't changed twice, which should never happen, so throws error if it does
combChanges :: NewRows -> PackedTree -> PackedTree
--combChanges changes initMat | trace ("combChanges with matrices "++ show changes) False = undefined
combChanges changes initMat -- check for duplicates and throw an error
    | -1 `elem` (foldr (\(c, _) acc -> if c `elem` acc then -1 : c : acc else acc) [] changes) = error "multiple changes made to same node"
    | otherwise = (V.//) initMat changes

-- | Combine changes to a tree from left and right children, multiple changes to the same node are not allowed
combTreeChanges :: NewNodes -> PhyloComponent -> PhyloComponent
combTreeChanges changes origTree 
    | -1 `elem` (foldr (\(c, _) acc -> if c `elem` acc then -1 : c : acc else acc) [] changes) = error "multiple changes made to same node"
    | otherwise = (V.//) origTree changes

-- | Wrapper for up pass recursion to deal with root
optimizationUpPass :: ExpandTree -> PackedInfo -> BN.PackMode -> TreeInfo
--optimizationUpPass (tree, mat, fmat) pInfo pMode | trace "up pass" False = undefined
optimizationUpPass (tree, mat, fmat) pInfo pMode 
    | isTerminal root = (tree, mat)
    | (length $ children root) > 2 = error "up pass cannot be performed for larger than binary trees" -- check for non-binary trees
    | (length $ children root) == 1 = -- for one child, recurse down and resolve changes
        let 
            leftCode = head $ children root
            leftChanges = internalUpPass (tree, mat, fmat) pInfo leftCode pMode
            outmat = combChanges leftChanges mat
        in (tree, outmat)
    | otherwise =  -- for two children, recurse down on both and resolve changes
        let 
            leftCode = head $ children root
            rightCode = head $ tail $ children root
            leftChanges = internalUpPass (tree, mat, fmat) pInfo leftCode pMode
            rightChanges = internalUpPass (tree, mat, fmat) pInfo rightCode pMode
            outmat = combChanges (leftChanges ++ rightChanges) mat
        in (tree, outmat)

        where root = head $ [x | x <- (V.toList tree), isRoot x]

-- | Internal up pass that performs most of the recursion
internalUpPass :: ExpandTree -> PackedInfo -> Int -> BN.PackMode -> NewRows
--internalUpPass (tree, mat, fmat) pInfo myCode pMode | trace ("internal up pass ") False = undefined
internalUpPass (tree, mat, fmat) pInfo myCode pMode
    | isTerminal (tree V.! myCode) = []
    | (length $ children $ (tree V.! myCode)) > 2 = error "up pass cannot be performed for larger than binary trees" -- check for non-binary
    | (length $ children $ (tree V.! myCode)) == 1 = --if one child, just recurse down
        let 
            node = tree V.! myCode
            leftCode = head $ children node
            leftChanges = internalUpPass (tree, mat, fmat) pInfo leftCode pMode
        in leftChanges
    | otherwise = --of two children, determine the final assignment and then recurse down before resolving changes
        let 
            node = tree V.! myCode
            leftCode = head $ children node
            rightCode = head $ tail $ children node
            parentCode = head $ parents $ tree V.! myCode
            newBit = upPassBitOps (mat V.! parentCode) (mat V.! myCode) (mat V.! leftCode) (mat V.! rightCode) (fmat V.! myCode) pInfo 
            passMat = (V.//) mat [(myCode, newBit)] 
            leftChanges = internalUpPass (tree, passMat, fmat) pInfo leftCode pMode
            rightChanges = internalUpPass (tree, passMat, fmat) pInfo rightCode pMode
        in (myCode, newBit) : (leftChanges ++ rightChanges)

-- | Bit operations for the up pass
upPassBitOps :: BN.BitPackedNode -> BN.BitPackedNode ->BN.BitPackedNode -> BN.BitPackedNode -> BN.BitPackedNode -> PackedInfo -> BN.BitPackedNode
upPassBitOps pBit myBit lBit rBit fBit pInfo = 
    let 
        setX = (BN.complement myBit) BN..&. pBit
        notX = BN.complement setX
        setG = notX BN..&. (snd $ masks pInfo)
        rightG = BN.blockShiftAndFold "R" "&" notX (blockLenMap pInfo) (length $ maxAlphabet pInfo) setG
        finalG = BN.blockShiftAndFold "L" "|" rightG (blockLenMap pInfo) (length $ maxAlphabet pInfo) rightG
        maskedNotG = (fst $ masks pInfo) BN..&. (BN.complement finalG)
        maskedNotF = (fst $ masks pInfo) BN..&. (BN.complement fBit)
        setS = myBit BN..&. (pBit BN..|. maskedNotG)
        sndS = setS BN..|. (pBit BN..&. fBit)
        thdS = sndS BN..|. (maskedNotG BN..&. (maskedNotF BN..&. (pBit BN..&. (lBit BN..|. rBit))))
    in thdS

-- | Function to get the root cost of the tree
getRootCost :: PhyloComponent -> V.Vector Float
--getRootCost nodes | trace ("nodes " ++ show nodes) False = undefined
getRootCost nodes = 
    let root = head $ [x | x <- (V.toList nodes), isRoot x]
    in totalCost root
