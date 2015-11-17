{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IATests (main) where

import Prelude
import Data.Vector hiding ((++), length, foldr, null)
import qualified Data.Vector as V ((++))
import qualified Component as PN
import Data.List (intersect)
import CharacterData (BaseChar)
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ImpliedAlign
import Data.Matrix hiding (fromList)
import CharacterData (CharacterSetList)

testTreeLen = 100

main :: IO()
main = defaultMain subtreeVerify

-- | Unit tests on small functions
--smallTests :: Test
--smallTests = testGroup "Tests of all small functions" []

subtreeVerify :: TestTree
subtreeVerify = testGroup "Check correct generation of subtrees" [compareKnown, compareList, subLength, correctOnes]
    where
        compareKnown = testProperty "Returns a tree of correct length" checkTree
            where
                checkTree :: SimpleTree -> Bool
                checkTree (Leaf) = True
                checkTree (Node _ _) = True 
        compareList = testProperty "Nonzero length" checkList
            where
                checkList :: IndexTree -> Bool
                checkList tree = not $ null tree
        --    where
        --        getCost :: PN.PhyloComponent -> PackedTree -> BitPackedNode -> Bool
        --        getCost tree packedTree mask = (length cost) == 1
        --            where 
        --                redonePack = reindex tree packedTree
        --                numChars = (length $ redonePack ! 0) `div` 4
        --                info = PackedInfo empty empty empty mempty "ACGT-" numChars empty (mask, mask)
        --                mode = PackMode 16 False
        --                cost = costForest [tree] (redonePack, info, mode) 1.0

        --                reindex :: PN.PhyloComponent -> PackedTree -> PackedTree
        --                reindex tree pack =
        --                    let 
        --                        numLeaves = foldr (\node acc -> if isTerminal node then acc + 1 else acc) 0 tree
        --                        pickRows = take numLeaves pack
        --                        out = zipWith (\node row -> if isTerminal node then row else empty) tree pack

        subLength = testProperty "Subtrees return matrix of correct length" subLen
            where
                subLen :: PN.PhyloComponent -> Bool
                subLen tree = nrows subtrees == (length tree) && ncols subtrees == (length tree)
                    where subtrees = subtreeWrap tree
        correctOnes = testProperty "Has the 2(n-1) - 2 ones where n is the number of nodes" subContent
            where
                subContent :: PN.PhyloComponent -> Bool
                subContent tree = 
                    let 
                        n = length tree
                        matrix = subtreeWrap tree
                    in trace ("matrix "++ show matrix) (foldr (+) 0 matrix == (2 * (n-1)) - 2)
    

instance Arbitrary BaseChar where
    arbitrary = fmap fromList $ listOf $ choose (0, maxBound :: Int64)

    data BranchPhylo = PLeaf {code :: PN.NodeCode                --links to DataMatrix for terminal
                            , nodeName :: String            --fromNameList HTUcode for non-leaf, or Newick
                            , isTerminal :: Bool --removed in favor of checking for null and one children/parents
                            , isRoot :: Bool
                            , isTreeNode :: Bool
                            , children :: [Int]
                            , parents :: [PN.NodeCode]
                            , preliminaryStates :: !CharacterSetList
                            , localCost :: !(Vector Float)
                            , totalCost :: !(Vector Float)
                            , preliminaryGapped :: BaseChar
                            , alignLeft :: BaseChar
                            , alignRight :: BaseChar}
                        | PNode {code :: PN.NodeCode                --links to DataMatrix for terminal
                            , nodeName :: String            --fromNameList HTUcode for non-leaf, or Newick
                            , isTerminal :: Bool --removed in favor of checking for null and one children/parents
                            , isRoot :: Bool
                            , isTreeNode :: Bool
                            , children :: [PN.NodeCode] -- change to int sets for children and parents?
                            , parents :: [PN.NodeCode]
                            , preliminaryStates :: !CharacterSetList
                            , localCost :: !(Vector Float)
                            , totalCost :: !(Vector Float)
                            , preliminaryGapped :: BaseChar
                            , alignLeft :: BaseChar
                            , alignRight :: BaseChar
                            , leftChild :: BranchPhylo
                            , rightChild :: BranchPhylo}

instance Arbitrary BranchPhylo where
    arbitrary = do
        terminate <- choose (0,1) :: Gen Int
        leftTree <- arbitrary :: Gen BranchPhylo
        rightTree <- arbitrary :: Gen BranchPhylo
        mySeq <- listOf (arbitrary :: Gen BaseChar)
        myGapped <- arbitrary :: Gen BaseChar
        alignLeft <- arbitrary :: Gen BaseChar
        alignRight <- arbitrary :: Gen BaseChar
        genCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
        totalCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
        if terminate == 0 then return $ PLeaf 0 "hi" True False False [] [] mySeq genCost totalCost myGapped alignLeft alignRight
            else return $ PNode 0 "hi" False False True [] [] mySeq genCost totalCost myGapped alignLeft alignRight leftTree rightTree

instance Arbitrary PN.PhyloComponent where
    arbitrary = fmap (indexPhylo empty) (arbitrary :: Gen BranchPhylo)

indexPhylo :: PN.PhyloComponent -> BranchPhylo -> PN.PhyloComponent
indexPhylo initTree newLeaf
    | null initTree && isTerminal newLeaf =
        let outLeaf = PN.PhyloNode 0 (nodeName newLeaf) True True False (children newLeaf) []
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
        in initTree V.++ (singleton outLeaf)
    | null initTree = 
        let outLeaf = PN.PhyloNode 0 (nodeName newLeaf) False True True (children newLeaf) []
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
        in initTree V.++ (singleton outLeaf)
    | isTerminal newLeaf = 
        let outLeaf = PN.PhyloNode (length initTree) (nodeName newLeaf) True (isRoot newLeaf) (isTreeNode newLeaf) [] [length initTree - 1]
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
        in initTree V.++ (singleton outLeaf)
    | otherwise = 
        let
            partNode = PN.PhyloNode (length initTree) (nodeName newLeaf) False False True [] [length initTree - 1]
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
            leftTree = indexPhylo (initTree V.++ (singleton partNode)) (leftChild newLeaf) 
            rightTree = indexPhylo leftTree (rightChild newLeaf)
            finalNode = partNode {PN.children = [length initTree + 1, length leftTree + 1]}
        in rightTree // [(length initTree, finalNode)]

