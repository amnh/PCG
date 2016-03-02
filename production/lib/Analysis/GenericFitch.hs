{-# LANGUAGE ConstraintKinds #-}
{- Module for non-additive optimization of a bit packed tree-}

module Analysis.GenericFitch where

-- imports
import Bio.Phylogeny.Tree.Binary
import Bio.Phylogeny.Tree.CharacterAware
import Bio.Phylogeny.Tree.Node.Encoded
import Bio.Phylogeny.Tree.Node.Preliminary
import Bio.Phylogeny.Tree.Node.Final
import Bio.Phylogeny.Network
import Bio.Sequence.Coded
import Bio.Phylogeny.PhyloCharacter


import qualified Data.Vector as V
import Debug.Trace
import Data.Maybe
import Data.Bits
import qualified Data.IntMap as IM

type TreeConstraint t n s b = (BinaryTree t n, Show t, NodeConstraint n s b, CharacterTree t s)
type NodeConstraint n s b = (EncodedNode n s, PreliminaryNode n s, FinalNode n s, SeqConstraint s b, Show n)
type SeqConstraint s b = (Bits b, CodedSequence s b, Bits s, Show s)

-- | Unified function to perform both the first and second passes of fitch
allOptimization :: TreeConstraint t n s b => Double -> t -> t
--allOptimization _ inTree | trace ("allOptimization " ++ show (names inTree IM.! 63)) False = undefined
allOptimization weight inTree = 
    let 
        downPass = optimizationPreorder weight inTree
        upPass = optimizationPostorder downPass
    in upPass

-- | Optimization down pass warpper for recursion from root
optimizationPreorder :: TreeConstraint t n s b => Double -> t -> t
optimizationPreorder weight tree
    | isLeaf (root tree) tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let 
            newNode = setCost 0.0 (root tree)
            newTree = tree `update` [newNode]
        in newTree
    | leftOnly && rightOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            newNodes = (setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) (root tree)) : nodes1
        in tree `update` newNodes
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) (root tree)
            newNodes = myNode : nodes1
        in tree `update` newNodes
    | otherwise = 
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild (root tree) tree) tree 
            nodes2 = internalPreorder weight (fromJust $ leftChild (root tree) tree) tree
            myNode = preorderBitOps weight (root tree) (head nodes1) (head nodes2) (characters tree)
            newNodes = myNode : (nodes1 ++ nodes2)
        in tree `update` newNodes

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal down pass that creates new rows without combining, making the algorithm faster
internalPreorder :: TreeConstraint t n s b => Double -> n -> t -> [n]
internalPreorder weight node tree 
    | isLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) node
        in myNode : nodes1
    | leftOnly = 
        let 
            nodes1 = internalPreorder weight (fromJust $ leftChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) node
        in myNode : nodes1
    | otherwise = 
        let 
            nodes1 = internalPreorder weight (fromJust $ rightChild node tree) tree 
            nodes2 = internalPreorder weight (fromJust $ leftChild node tree) tree
            myNode = preorderBitOps weight node (head nodes1) (head nodes2) (characters tree)
        in myNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Bit operations for the down pass: basically creats a mask for union and intersection areas and then takes them
-- returns the new assignment, the union/intersect mask, and the new total cost
preorderBitOps :: NodeConstraint n s b => Double -> n -> n -> n -> V.Vector (PhyloCharacter s) -> n
--preorderBitOps _ _ lNode _ treeChars | trace ("preorderBitOps " ++ show treeChars ++ show lNode) False = undefined
preorderBitOps weight curNode lNode rNode treeChars =
    let
        lbit = grabAligned treeChars lNode
        chars = V.filter aligned treeChars
        rbit = grabAligned treeChars rNode
        notOr = complement $ lbit .&. rbit
        union = lbit .|. rbit
        fbit = notOr .&. V.map (snd . fitchMasks) chars
        rightF = blockShiftAndFold "R" "&" chars notOr fbit
        finalF = blockShiftAndFold "L" "|" chars rightF rightF
        maskF = V.map (fst . fitchMasks) chars .&. finalF
        myCost = fetchCost maskF chars
        weightCost = weight * myCost
        totalCost = cost lNode + cost rNode + weightCost
        outbit = (maskF .&. union) .|. (lbit .&. rbit)
        outNode = setPreliminary outbit $ setAlign outbit $ setTemporary finalF $ setCost totalCost curNode
    in outNode

    where
        fetchCost :: SeqConstraint s b => V.Vector s -> V.Vector (PhyloCharacter s) -> Double
        fetchCost encodeIn chars 
            | V.length chars /= V.length encodeIn = 0
            | otherwise = 
                let val = V.ifoldr (\i char acc -> div (popCount char) (length $ alphabet $ chars V.! i) + acc) 0 encodeIn
                in (fromIntegral val :: Double)

-- | Wrapper for up pass recursion to deal with root
optimizationPostorder :: TreeConstraint t n s b => t -> t
optimizationPostorder tree 
    | isLeaf (root tree) tree = tree
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = 
        let nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree
        in tree `update` nodes1
    | leftOnly = 
        let nodes1 = internalPostorder (fromJust $ leftChild (root tree) tree) tree
        in tree `update` nodes1
    | otherwise = 
        let
            nodes1 = internalPostorder (fromJust $ rightChild (root tree) tree) tree
            nodes2 = internalPostorder (fromJust $ leftChild (root tree) tree) tree
        in tree `update` (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal up pass that performs most of the recursion
internalPostorder :: TreeConstraint t n s b => n -> t -> [n]
internalPostorder node tree 
    | isLeaf node tree = []
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = internalPostorder (fromJust $ rightChild node tree) tree
    | leftOnly = internalPostorder (fromJust $ leftChild node tree) tree
    | otherwise = 
        let 
            nodes1 = internalPostorder (fromJust $ rightChild node tree) tree
            nodes2 = internalPostorder (fromJust $ leftChild node tree) tree
            newNode = postorderBitOps node (fromJust $ leftChild node tree) (fromJust $ rightChild node tree) (parent node tree) (characters tree)
        in newNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree


-- | Bit operations for the up pass
postorderBitOps :: NodeConstraint n s b => n -> n -> n -> Maybe n -> V.Vector (PhyloCharacter s) -> n
postorderBitOps myNode lNode rNode pNodeMaybe treeChars
    | isNothing pNodeMaybe = error "No parent node on postorder bit operations"
    | otherwise =  
        let
            pNode = fromJust pNodeMaybe
            lBit = grabAligned treeChars lNode
            chars = V.filter aligned treeChars
            rBit = grabAligned treeChars rNode
            myBit = grabAligned treeChars myNode
            fBit = V.ifilter (\i _ -> aligned $ chars V.! i) (temporary myNode)
            pBit = grabAligned treeChars pNode
            setX = complement myBit .&. pBit
            notX = complement setX
            setG = notX .&. V.map (snd . fitchMasks) chars
            rightG = blockShiftAndFold "R" "&" chars notX setG
            finalG = blockShiftAndFold "L" "|" chars rightG rightG
            fstMask = V.map (fst . fitchMasks) chars
            maskedNotG = complement finalG .&. fstMask
            maskedNotF = complement fBit   .&. fstMask
            setS = myBit .&. (pBit .|. maskedNotG)
            sndS = setS .|. (pBit .&. fBit)
            thdS = sndS .|. (maskedNotG .&. (maskedNotF .&. (pBit .&. (lBit .|. rBit))))
        in setFinal thdS myNode

-- | Grabs the aligned portions of a node's encoded sequence
grabAligned :: NodeConstraint n s b => V.Vector (PhyloCharacter s) -> n -> V.Vector s
--grabAligned node treeChars | trace ("grab aligned " ++ show node ++ show treeChars) False = undefined
grabAligned treeChars inNode
    | not $ null (preliminary inNode) = filt (preliminary inNode)
    | not $ null (encoded inNode)     = filt (encoded inNode)
    | otherwise = error ("Node " ++ show inNode ++ " has no sequence for Fitch")
    where
        filt = V.ifilter (\i _ -> aligned $ treeChars V.! i)

-- | Convenience function for bit ops
blockShiftAndFold :: SeqConstraint s b => String -> String -> V.Vector (PhyloCharacter s) -> V.Vector s -> V.Vector s -> V.Vector s
blockShiftAndFold sideMode foldMode chars inbits initVal 
    | sideMode == "L" && foldMode == "&" = f (.&.)
    | sideMode == "R" && foldMode == "&" = f (.&.)
    | sideMode == "L" && foldMode == "|" = f (.|.)
    | sideMode == "R" && foldMode == "|" = f (.|.)
    | otherwise = error "incorrect input for block shift and fold"
    where
      f g = V.zipWith3 (\b c iVal -> foldr (\s acc -> g acc (shiftL b s)) iVal [1 .. length (alphabet c) - 1]) inbits chars initVal
