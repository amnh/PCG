{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IATests (main, processFiles) where

import Data.Vector hiding ((++), length, foldr, null, zipWith, and, head, foldr, reverse, sequence, filter, tail, map)
import qualified Data.Vector as V ((++), zipWith, and, head, length, foldr, reverse, toList, filter, tail, map)
import qualified Component as PN
import Data.List (intersect, reverse)
import CharacterData (BaseChar)
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ImpliedAlign
import Data.Matrix hiding (fromList, trace, toList, (!))
import CharacterData (CharacterSetList)
import Debug.Trace
import Packing.PackedTest (getSeqsFromFile, getTreeFromFile)
import Packing.PackedOptimize
import Data.Traversable (sequence)
import ReadFiles
import Data.Bits (popCount)
import Packing.PackedBuild
import Packing.BitPackedNode
import Data.BitVector (nat, bit)

testTreeLen = 100

main :: IO()
main = do
    unit <- simpleUnit
    defaultMain (testGroup "Tests of Implied Alignment" [subtreeVerify, iaVerify, unit])

subtreeVerify :: TestTree
subtreeVerify = testGroup "Check correct generation of subtrees" [subLength, correctOnes]
    where

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
                        numOnes = foldr (+) 0 (subtreeWrap tree)
                    in --trace ("Actual number of ones " ++ show numOnes ++ " with n " ++ show n) 
                        ((numOnes == (2 * n) - 4) || numOnes == 0)

iaVerify :: TestTree
iaVerify = testGroup "Check properties of implied alignment" [mergeLength, conserveProperties, conserveSize, seqLensIncrease]
    where
        mergeLength = testProperty "Merged subtrees have correct length" checkMerge
        conserveProperties = testProperty "Other properties are conserved before and after alignment" checkExternal
        conserveSize = testProperty "Number of nodes is conserved" checkSize
        seqLensIncrease = testProperty "Sequence lengths are the same or longer" checkSeqs
        standardInfo = CharInfo GenSeq False 1.0 [] "hi" 5 ["A","C", "G", "T", "-"] 0.0

        checkMerge :: PN.PhyloComponent -> PN.PhyloComponent -> Bool
        checkMerge tree1 tree2 = 
            let node = V.head tree1
            in (V.length $ mergeSubtree tree1 tree2 node) == (V.length tree1) + (V.length tree2) + 1

        checkExternal :: PN.PhyloComponent -> Bool
        checkExternal tree = 
            let 
                ia = implyMain standardInfo tree
                matches = V.zipWith matchProperties ia tree
            in --trace ("matches " ++ show matches ++ " with nodes " ++ show ia ++ show tree)
                V.and matches
        checkSize :: PN.PhyloComponent -> Bool
        checkSize tree = 
            let ia = implyMain standardInfo tree
            in V.length ia == V.length tree

        checkSeqs :: PN.PhyloComponent -> Bool
        checkSeqs tree = 
            let 
                ia = implyMain standardInfo tree
                matchLens = V.zipWith matchLengths ia tree
            in V.and matchLens


        matchProperties :: PN.PhyloNode -> PN.PhyloNode -> Bool
        matchProperties n1 n2 = PN.code n1 == PN.code n2 && PN.nodeName n1 == PN.nodeName n2 && PN.isTerminal n1 == PN.isTerminal n2
                                    && PN.isRoot n1 == PN.isRoot n2 && PN.isTreeNode n1 == PN.isTreeNode n2 
                                    && PN.children n1 == PN.children n2 && PN.parents n1 == PN.parents n2 
                                    && PN.localCost n1 == PN.localCost n2 && PN.totalCost n1 == PN.totalCost n2

        matchLengths :: PN.PhyloNode -> PN.PhyloNode -> Bool
        matchLengths n1 n2 = (getPop $ PN.preliminaryGapped n2) >= (getPop $ PN.preliminaryGapped n1) 
                                && (getPop $ PN.alignLeft n2) >= (getPop $ PN.alignLeft n1)
                                && (getPop $ PN.alignRight n2) >= (getPop $ PN.alignRight n1)
            where
                getPop :: Vector Int64 -> Int
                getPop = V.foldr (\i acc -> acc + popCount i) 0

simpleUnit :: IO TestTree
simpleUnit = (testGroup "Simple unit test") <$> (sequence [treeCase])
    where
        standardInfo = CharInfo GenSeq False 1.0 [] "hi" 5 ["A","C", "G", "T", "-"] 0.0
        standardNode = PN.PhyloNode 0 "stop" False False False [] [] [] empty empty empty empty empty empty
        --simpleTree = fromList [standardNode {PN.isRoot = True, PN.children = [1,2], PN.preliminaryGapped = fromList [8, 1], PN.alignLeft = fromList [1, 2, 4], PN.alignRight = fromList [8], PN.preliminaryStates = [fromList [8, 1]]}
        --                , standardNode {PN.isTerminal = True, PN.preliminaryGapped = fromList [1, 2, 4], PN.code = 1, PN.parents = [0], PN.preliminaryStates = [fromList [1, 2, 4]]}
        --                , standardNode {PN.isTerminal = True, PN.preliminaryGapped = fromList [8], PN.code = 2, PN.parents = [0], PN.preliminaryStates = [fromList [8]]}]
        --resultTree = fromList [standardNode {PN.isRoot = True, PN.children = [1,2], PN.preliminaryGapped = fromList [8, 1, 16, 16], PN.alignLeft = fromList [16, 1, 2, 4], PN.alignRight = fromList [8, 16, 16, 16], PN.preliminaryStates = [fromList [8, 1]]}
        --                , standardNode {PN.isTerminal = True, PN.preliminaryGapped = fromList [16, 1, 2, 4], PN.code = 1, PN.parents = [0], PN.preliminaryStates = [fromList [1, 2, 4]]}
        --                , standardNode {PN.isTerminal = True, PN.preliminaryGapped = fromList [8, 16, 16, 16], PN.code = 2, PN.parents = [0], PN.preliminaryStates = [fromList [8]]}]
        resultTree = fromList [standardNode {PN.code = 0, PN.nodeName = "#a", PN.isTerminal = False, PN.isRoot = True, PN.isTreeNode = True, PN.children = [1,2], PN.parents = [], PN.preliminaryStates = [], PN.totalCost = fromList [2.0], PN.preliminaryGapped = fromList [13568], PN.alignLeft = fromList [9216], PN.alignRight = fromList [4352]},
                                standardNode {PN.code = 1, PN.nodeName = "b", PN.isTerminal = True, PN.isRoot = False, PN.isTreeNode = False, PN.children = [], PN.parents = [0], PN.preliminaryStates = [], PN.totalCost = fromList [0.0], PN.preliminaryGapped = fromList [9216], PN.alignLeft = fromList [], PN.alignRight = fromList []},
                                standardNode {PN.code = 2, PN.nodeName = "c", PN.isTerminal = True, PN.isRoot = False, PN.isTreeNode = False, PN.children = [], PN.parents = [0], PN.preliminaryStates = [], PN.totalCost = fromList [0.0], PN.preliminaryGapped = fromList [4352], PN.alignLeft = fromList [], PN.alignRight = fromList []}]
        readTreeInfo = processFiles "TinyCommands.pcg"
        singleRead = fmap head readTreeInfo
        ioTree = fmap assignNodes singleRead
        treeEquiv = (assertEqual "Check tree stays the same" resultTree) <$> ioTree
        treeCase = (testCase "Check evaluation") <$> treeEquiv


processFiles :: String -> IO [TreeInfo]
processFiles inFile = do
    seqs <- getSeqsFromFile "TinySequence.fas"
    tree <- getTreeFromFile "TinyTree.newick"
    let weight = 1
    let names  = head $ filter (not.null) . fmap PN.nodeName <$> fmap V.toList tree
    return $ optimizeForest tree (performPack seqs names tree ("static", "16")) weight

assignNodes :: TreeInfo -> PN.PhyloComponent
--assignNodes (inTree, pack) | trace ("assign nodes " ++ show pack) False = undefined
assignNodes (inTree, pack) = --undefined
    let rootCode = PN.code $ V.head $ V.filter PN.isRoot inTree
    in inTree // (assign rootCode (inTree, pack))

    where 
        assign :: Int -> TreeInfo -> [(Int, PN.PhyloNode)]
        assign curNode (inTree, pack) 
            | PN.isTerminal (inTree ! curNode) = [(curNode, (inTree ! curNode) {PN.preliminaryGapped = (packToBase $ pack ! curNode)})]
            | (length $ PN.children (inTree ! curNode)) < 2 = error "Only binary trees allowed"
            | otherwise = 
                let 
                    leftCode = head $ PN.children (inTree ! curNode)
                    rightCode = head $ tail $ PN.children (inTree ! curNode)
                    newNode = (inTree ! curNode) {PN.preliminaryGapped = (packToBase $ pack ! curNode), PN.alignLeft = (packToBase $ pack ! leftCode), PN.alignRight = (packToBase $ pack ! rightCode)}
                    leftUpdates = assign leftCode (inTree, pack)
                    rightUpdates = assign rightCode (inTree, pack)
                in [(curNode, newNode)] ++ leftUpdates ++ rightUpdates

        packToBase :: BitPackedNode -> BaseChar
        packToBase input | trace ("input to conversion "++ show input) False = undefined
        packToBase EmptyPackNode = empty
        packToBase (S16 content) = --trace (show (fromEnum $ V.head content :: Int64))
                                    V.map (\i -> fromIntegral i) content :: Vector Int64
        packToBase (S64 content) = V.map fromIntegral content :: Vector Int64
        packToBase (A16 content) = V.map fromIntegral content :: Vector Int64
        packToBase (A64 content) = V.map fromIntegral content :: Vector Int64
        packToBase (SInf content) = singleton $ fromIntegral $ nat content :: Vector Int64

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
                            , alignRight :: BaseChar
                            , tempField :: BaseChar}
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
                            , rightChild :: BranchPhylo
                            , tempField :: BaseChar}

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
        if terminate == 0 then return $ PLeaf 0 "hi" True False False [] [] mySeq genCost totalCost myGapped empty empty empty
            else return $ PNode 0 "hi" False False True [] [] mySeq genCost totalCost myGapped alignLeft alignRight leftTree rightTree empty

instance Arbitrary PN.PhyloComponent where
    arbitrary = fmap (indexPhylo empty) (arbitrary :: Gen BranchPhylo)

indexPhylo :: PN.PhyloComponent -> BranchPhylo -> PN.PhyloComponent
indexPhylo initTree newLeaf
    | null initTree && isTerminal newLeaf =
        let outLeaf = PN.PhyloNode 0 (nodeName newLeaf) True True False (children newLeaf) []
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf) (tempField newLeaf)
        in initTree V.++ (singleton outLeaf)
    | null initTree = 
        let outLeaf = PN.PhyloNode 0 (nodeName newLeaf) False True True (children newLeaf) []
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf) (tempField newLeaf)
        in initTree V.++ (singleton outLeaf)
    | isTerminal newLeaf = 
        let outLeaf = PN.PhyloNode (length initTree) (nodeName newLeaf) True (isRoot newLeaf) (isTreeNode newLeaf) [] [length initTree - 1]
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf) (tempField newLeaf)
        in initTree V.++ (singleton outLeaf)
    | otherwise = 
        let
            partNode = PN.PhyloNode (length initTree) (nodeName newLeaf) False False True [] [length initTree - 1]
                                (preliminaryStates newLeaf) (localCost newLeaf) (totalCost newLeaf) 
                                (preliminaryGapped newLeaf) (alignLeft newLeaf) (alignRight newLeaf) (tempField newLeaf)
            leftTree = indexPhylo (initTree V.++ (singleton partNode)) (leftChild newLeaf) 
            rightTree = indexPhylo leftTree (rightChild newLeaf)
            finalNode = partNode {PN.children = [length initTree + 1, length leftTree + 1]}
        in rightTree // [(length initTree, finalNode)]

