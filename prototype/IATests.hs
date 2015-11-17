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

data SimpleTree = Leaf | Node SimpleTree SimpleTree deriving (Show)

instance Arbitrary SimpleTree where
    arbitrary = do
        terminate <- choose (0,1) :: Gen Int
        leftTree <- arbitrary :: Gen SimpleTree
        rightTree <- arbitrary :: Gen SimpleTree
        if terminate == 0 then return Leaf
            else return $ Node leftTree rightTree

type IndexTree = [SimpleTree]

instance Arbitrary IndexTree where
    arbitrary = fmap (traverseIndex [])  (arbitrary :: Gen SimpleTree)

traverseIndex :: IndexTree -> SimpleTree -> IndexTree
traverseIndex initTree n@(Node left right) = 
    let
        update = n : initTree
        leftList = traverseIndex update left
        rightList = traverseIndex (leftList ++ update) right
    in rightList ++ leftList ++ update
traverseIndex initTree Leaf = Leaf : initTree

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

--indexPhylo initTree l@(PNode _ _ _ True _ _ _ _ _ _ _ _ _ _ _) = 
--    let 
--        newLeaf = l
--        phyloLeaf = PN.PhyloNode 0 (nodeName newLeaf) True (isRoot newLeaf) (isTreeNode newLeaf) (children newLeaf) []
--                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
--                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
--    in initTree V.++ (singleton phyloLeaf)
--indexPhylo initTree l@(PLeaf _ _ _ _ _ _ _ _ _ _ _ _ _) = 
--    let 
--        newLeaf = l {code = (length initTree), parents = [length initTree - 1]}
--        phyloLeaf = PN.PhyloNode (code newLeaf) (nodeName newLeaf) True (isRoot newLeaf) (isTreeNode newLeaf) [] (parents newLeaf)
--                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
--                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf)
--    in initTree V.++ (singleton phyloLeaf)
--indexPhylo initTree n@(PNode _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = 
--    let 
--        partNode = PN.PhyloNode (length initTree) (nodeName n) False (isRoot n)
--                        (isTreeNode n) [] [length initTree - 1]
--                        (preliminaryStates n) (localCost n) (totalCost n) 
--                        (preliminaryGapped n) (alignLeft n) (alignRight n)
--        leftTree = indexPhylo (initTree V.++ (singleton partNode)) (leftChild n) 
--        rightTree = indexPhylo leftTree (rightChild n)
--        finalNode = partNode {PN.children = [length initTree + 1, length leftTree + 1]}
--    in rightTree // [(length initTree, finalNode)]

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
    

--instance Arbitrary PN.PhyloComponent where
--    --arbitrary = fmap fromList $ (vectorOf testTreeLen (arbitrary :: Gen PhyloNode)) `suchThat` (any isRoot) 
--    arbitrary = do
--        treeLen <- choose (3,testTreeLen) :: Gen Int
--        let children = [1, 2]
--        premStates <- listOf (arbitrary :: Gen BaseChar)
--        localCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        totalCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        preliminaryGapped <- arbitrary :: Gen BaseChar
--        alignLeft <- arbitrary :: Gen BaseChar
--        alignRight <- arbitrary :: Gen BaseChar

--        let root = PhyloNode 0 "root" False True False children [] premStates localCost totalCost preliminaryGapped alignLeft alignRight
--        splitPos <- choose (3, treeLen - 1) :: Gen Int
--        leftNodes <- arbNodeRecurse 0 (3, splitPos) 
--        rightNodes <- arbNodeRecurse 0 (splitPos + 1, treeLen - 1)
--        let tree = root `cons` (leftNodes V.++ rightNodes)
--        return tree


--arbNodeRecurse :: Int -> (Int, Int) -> Gen PN.PhyloComponent
--arbNodeRecurse parentCode (startPos, stopPos) 
--    | startPos == stopPos = do
--        mySeq <- listOf (arbitrary :: Gen BaseChar)
--        myGapped <- arbitrary :: Gen BaseChar
--        alignLeft <- arbitrary :: Gen BaseChar
--        alignRight <- arbitrary :: Gen BaseChar
--        return $ singleton $ PhyloNode startPos (show startPos) True False False [] [parentCode] mySeq (singleton 0) (singleton 0) myGapped alignLeft alignRight
--    | otherwise = do
--        mySeq <- listOf (arbitrary :: Gen BaseChar)
--        myGapped <- arbitrary :: Gen BaseChar
--        alignLeft <- arbitrary :: Gen BaseChar
--        alignRight <- arbitrary :: Gen BaseChar
--        sideSplit <- choose (startPos + 1, stopPos - 1) :: Gen Int
--        genCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        totalCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        left <- arbNodeRecurse startPos (startPos + 1, sideSplit)
--        right <- arbNodeRecurse startPos (sideSplit + 1, stopPos)
--        return $ (PhyloNode startPos (show startPos) False False True [startPos + 1, sideSplit + 1] [parentCode] mySeq genCost totalCost myGapped alignLeft alignRight) 
--                    `cons` (left V.++ right)

-- | Non-recursive instance
--instance Arbitrary PhyloNode where
--    arbitrary = do 
--        numChildren <- 
--        code <- arbitrary :: Gen Int
--        nodeName <- arbitrary :: Gen String
--        isTerminal <- arbitrary :: Gen Bool
--        isRoot <- arbitrary `suchThat` ((not isTerminal) &&) :: Gen Bool
--        isTreeNode <- arbitrary `suchThat` ((not $ isTerminal || isRoot) &&) :: Gen Bool
--        children' <- (vectorOf numChildren $ (choose (0,testTreeLen) :: Gen Int) `suchThat` ((/=) code))
--        parents <- [(choose (0,testTreeLen) :: Gen Int) `suchThat` ((/=) code))] `suchThat` (null $ intersect children')
--        preliminaryStates <- listOf (arbitrary :: Gen BaseChar)
--        localCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        totalCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
--        preliminaryGapped <- arbitrary :: Gen BaseChar
--        alignLeft <- arbitrary :: Gen BaseChar
--        alignRight <- arbitrary :: Gen BaseChar
--        return $ PhyloNode code nodeName isTerminal isRoot isTreeNode children' parents preliminaryStates localCost totalCost preliminaryGapped alignLeft alignRight

--instance Arbitrary BitPackedNode where
--    arbitrary = fmap S16 $ fmap fromList $ listOf $ choose (0, maxBound :: Word16)

--instance Arbitrary PackedTree where
--    arbitrary = do
--        len <- choose (0, 100) :: Gen Int
--        return $ fmap fromList $ liftOf $ fmap S16 $ fmap fromList $ vectorOf len $ choose (0, maxBound :: Word16)

instance Arbitrary BaseChar where
    arbitrary = fmap fromList $ listOf $ choose (0, maxBound :: Int64)
