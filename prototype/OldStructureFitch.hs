{-# LANGUAGE ConstraintKinds #-}
{- Module for non-additive optimization of a bit packed tree-}

module OldStructureFitch where

-- imports 
import Data.Tree.Binary
import Data.Tree.Node.Encoded
import Data.Tree.Node.Preliminary
import Data.Tree.Node.Character
import Data.Tree.Node.Final
import qualified Data.Vector as V
import qualified Packing.BitPackedNode as BN
import Debug.Trace
import Data.Int
import Data.Maybe
import Safe
import Data.Sequence.Coded
import Data.Bits
import Data.PhyloCharacter
import Control.Monad

type TreeConstraint t n b = (EncodedNode n b, BinaryTree t n, PreliminaryNode n b, CharacterNode n b, Bits b, FinalNode n b, Show t)
type NodeConstraint n b = (EncodedNode n b, PreliminaryNode n b, CharacterNode n b, Bits b, FinalNode n b)

-- | Unified function to perform both the first and second passes of fitch
allOptimization :: TreeConstraint t n b => Float -> t -> t
--allOptimization weight inTree | trace ("allOptimization " ++ show inTree) False = undefined
allOptimization weight inTree = 
    let 
        downPass = optimizationDownPass weight inTree
        upPass = optimizationUpPass weight downPass
    in upPass

-- | Optimization down pass warpper for recursion from root
optimizationDownPass :: TreeConstraint t n b => Float -> t -> t
optimizationDownPass weight tree
    | isLeaf (root tree) tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let 
            newNode = setCost 0.0 (root tree)
            newTree = tree `update` [newNode]
        in newTree
    | leftOnly && rightOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalDownPass weight (fromJust $ rightChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) (root tree)
            newNodes = myNode : nodes1
        in tree `update` newNodes
    | leftOnly = 
        let 
            nodes1 = internalDownPass weight (fromJust $ leftChild (root tree) tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) (root tree)
            newNodes = myNode : nodes1
        in tree `update` newNodes
    | otherwise = 
        let 
            nodes1 = internalDownPass weight (fromJust $ rightChild (root tree) tree) tree 
            nodes2 = internalDownPass weight (fromJust $ leftChild (root tree) tree) tree
            myNode = downBitOps weight (root tree) (head nodes1) (head nodes2)
            newNodes = myNode : (nodes1 ++ nodes2)
        in tree `update` newNodes

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal down pass that creates new rows without combining, making the algorithm faster
internalDownPass :: TreeConstraint t n b => Float -> n -> t -> [n]
internalDownPass weight node tree 
    | isLeaf node tree = -- if the root is a terminal, give the whole tree a cost of zero, do not reassign nodes
        let newNode = setCost 0.0 node
        in [newNode]
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = -- if there is only one child, continue recursion down and resolve
        let 
            nodes1 = internalDownPass weight (fromJust $ rightChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) node
        in myNode : nodes1
    | leftOnly = 
        let 
            nodes1 = internalDownPass weight (fromJust $ leftChild node tree) tree -- with only one child, assignment and cost is simply carried up
            carryNode = head nodes1
            myNode = setTemporary (temporary carryNode) $ setAlign (preliminaryAlign carryNode) 
                        $ setPreliminary (preliminary carryNode) $ setCost (cost carryNode) node
        in myNode : nodes1
    | otherwise = 
        let 
            nodes1 = internalDownPass weight (fromJust $ rightChild node tree) tree 
            nodes2 = internalDownPass weight (fromJust $ leftChild node tree) tree
            myNode = downBitOps weight node (head nodes1) (head nodes2)
        in myNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree

-- | Bit operations for the down pass: basically creats a mask for union and intersection areas and then takes them
-- returns the new assignment, the union/intersect mask, and the new total cost
downBitOps :: NodeConstraint n b => Float -> n -> n -> n -> n
downBitOps weight curNode lNode rNode =
    let
        lbit = grabAligned lNode
        chars = V.filter aligned (characters lNode)
        rbit = grabAligned rNode
        notOr = complement $ lbit .&. rbit
        union = lbit .|. rbit
        fbit = notOr .&. (Just $ V.map (snd . masks) chars)
        rightF = blockShiftAndFold "R" "&" chars notOr fbit
        finalF = blockShiftAndFold "L" "|" chars rightF rightF
        maskF = (Just $ V.map (fst . masks) chars) .&. finalF
        myCost = fetchCost maskF chars
        weightCost = weight * myCost
        totalCost = (cost lNode) + (cost rNode) + weightCost
        outbit = (maskF .&. union) .|. (lbit .&. rbit)
    in setPreliminary outbit $ setAlign outbit $ setTemporary finalF $ setCost totalCost curNode

    where
        fetchCost :: (Bits b) => EncodedSeq b -> V.Vector (PhyloCharacter b) -> Float
        fetchCost encoded chars 
            | isNothing encoded || (V.length chars) /= (V.length $ fromJust encoded) = 0
            | otherwise = 
                let val = V.ifoldr (\i char acc -> div (popCount char) (V.length $ alphabet $ chars V.! i)) 0 (fromJust encoded)
                in (fromIntegral val :: Float)

-- | Wrapper for up pass recursion to deal with root
optimizationUpPass :: TreeConstraint t n b => Float -> t -> t
optimizationUpPass weight tree 
    | isLeaf (root tree) tree = tree
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = 
        let nodes1 = internalUpPass weight (fromJust $ rightChild (root tree) tree) tree
        in tree `update` nodes1
    | leftOnly = 
        let nodes1 = internalUpPass weight (fromJust $ leftChild (root tree) tree) tree
        in tree `update` nodes1
    | otherwise = 
        let
            nodes1 = internalUpPass weight (fromJust $ rightChild (root tree) tree) tree
            nodes2 = internalUpPass weight (fromJust $ leftChild (root tree) tree) tree
        in tree `update` (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild (root tree) tree
            rightOnly = isNothing $ leftChild (root tree) tree

-- | Internal up pass that performs most of the recursion
internalUpPass :: TreeConstraint t n b => Float -> n -> t -> [n]
internalUpPass weight node tree 
    | isLeaf node tree = []
    | rightOnly && leftOnly = error "Problem with binary tree structure: non-terminal has no children"
    | rightOnly = internalUpPass weight (fromJust $ rightChild node tree) tree
    | leftOnly = internalUpPass weight (fromJust $ leftChild node tree) tree
    | otherwise = 
        let 
            nodes1 = internalUpPass weight (fromJust $ rightChild node tree) tree
            nodes2 = internalUpPass weight (fromJust $ leftChild node tree) tree
            newNode = upPassBitOps weight node (fromJust $ leftChild node tree) (fromJust $ rightChild node tree) (parent node tree)
        in newNode : (nodes1 ++ nodes2)

        where
            leftOnly = isNothing $ rightChild node tree
            rightOnly = isNothing $ leftChild node tree


-- | Bit operations for the up pass
upPassBitOps :: NodeConstraint n b => Float -> n -> n -> n -> Maybe n -> n
upPassBitOps weight myNode lNode rNode pNode = 
    let
        lBit = grabAligned lNode
        chars = V.filter aligned (characters lNode)
        rBit = grabAligned rNode
        myBit = grabAligned myNode
        fBit | (V.length $ characters myNode) /= (V.length $ fromJust $ temporary myNode) = Nothing
             | otherwise = Just $ V.ifilter (\i b -> aligned $ (characters myNode) V.! i) (fromJust $ temporary myNode)
        pBit = join $ fmap grabAligned pNode 
        setX = (complement myBit) .&. pBit
        notX = complement setX
        setG = notX .&. (Just $ V.map (snd . masks) chars)
        rightG = blockShiftAndFold "R" "&" chars notX setG
        finalG = blockShiftAndFold "L" "|" chars rightG rightG
        fstMask = Just $ V.map (fst . masks) chars
        maskedNotG = (complement finalG) .&. fstMask
        maskedNotF = (complement fBit) .&. fstMask
        setS = myBit .&. (pBit .|. maskedNotG)
        sndS = setS .|. (pBit .&. fBit)
        thdS = sndS .|. (maskedNotG .&. (maskedNotF .&. (pBit .&. (lBit .|. rBit))))
    in setFinal thdS myNode

-- | Grabs the aligned portions of a node's encoded sequence
grabAligned :: NodeConstraint n b => n -> EncodedSeq b
grabAligned node 
    | isNothing $ preliminaryAlign node = Nothing
    | (V.length $ characters node) /= (V.length $ fromJust $ preliminaryAlign node) = Nothing
    | otherwise = Just $ V.ifilter (\i b -> aligned $ (characters node) V.! i) (fromJust $ preliminaryAlign node)

-- | Convenience function for bit ops
blockShiftAndFold :: (Bits b) => String -> String -> V.Vector (PhyloCharacter b) -> EncodedSeq b -> EncodedSeq b -> EncodedSeq b
blockShiftAndFold sideMode foldMode chars inbits initVal 
    | isNothing inbits || isNothing initVal = Nothing
    | sideMode == "L" && foldMode == "&" = Just $ V.zipWith3 (\b c iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..(V.length $ alphabet c)-1]) (fromJust inbits) chars (fromJust initVal)
    | sideMode == "R" && foldMode == "&" = Just $ V.zipWith3 (\b c iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..(V.length $ alphabet c)-1]) (fromJust inbits) chars (fromJust initVal)
    | sideMode == "L" && foldMode == "|" = Just $ V.zipWith3 (\b c iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..(V.length $ alphabet c)-1]) (fromJust inbits) chars (fromJust initVal)
    | sideMode == "R" && foldMode == "|" = Just $ V.zipWith3 (\b c iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..(V.length $ alphabet c)-1]) (fromJust inbits) chars (fromJust initVal)
    | otherwise = error "incorrect input for block shift and fold"
