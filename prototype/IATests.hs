{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IATests (main) where

import Data.Vector (fromList)
import Component
import Data.List (intersect)
import CharacterData (BaseChar)
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ImpliedAlign
import Data.Matrix hiding (fromList)

testTreeLen = 100

main :: IO()
main = defaultMain subtreeVerify

-- | Unit tests on small functions
--smallTests :: Test
--smallTests = testGroup "Tests of all small functions" []

subtreeVerify :: TestTree
subtreeVerify = testGroup "Check correct generation of subtrees" [subLength, correctOnes]
    where
        subLength = testProperty "Subtrees return matrix of correct length" subLen
            where
                subLen :: PhyloComponent -> Bool
                subLen tree = nrows subtrees == (length tree) && ncols subtrees == (length tree)
                    where subtrees = subtreeWrap tree
        correctOnes = testProperty "Has the 2(n-1) - 2 ones where n is the number of nodes" subContent
            where
                subContent :: PhyloComponent -> Bool
                subContent tree = 
                    let n = length tree
                    in foldr (+) 0 (subtreeWrap tree) == (2 * (n-1)) - 2

instance Arbitrary PhyloComponent where
    arbitrary = fmap fromList $ (vectorOf testTreeLen (arbitrary :: Gen PhyloNode)) `suchThat` (any isRoot) 

instance Arbitrary PhyloNode where
    arbitrary = do 
        code <- arbitrary :: Gen Int
        nodeName <- arbitrary :: Gen String
        isTerminal <- arbitrary :: Gen Bool
        isRoot <- arbitrary `suchThat` ((not isTerminal) &&) :: Gen Bool
        isTreeNode <- arbitrary `suchThat` ((not $ isTerminal || isRoot) &&) :: Gen Bool
        children' <- (vectorOf testTreeLen $ (choose (0,100) :: Gen Int) `suchThat` ((/=) code))
        parents <- (vectorOf testTreeLen $ (choose (0,100) :: Gen Int) `suchThat` ((/=) code)) `suchThat` (null . intersect children')
        preliminaryStates <- listOf (arbitrary :: Gen BaseChar)
        localCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
        totalCost <- fmap fromList $ listOf (arbitrary :: Gen Float)
        preliminaryGapped <- arbitrary :: Gen BaseChar
        alignLeft <- arbitrary :: Gen BaseChar
        alignRight <- arbitrary :: Gen BaseChar
        return $ PhyloNode code nodeName isTerminal isRoot isTreeNode children' parents preliminaryStates localCost totalCost preliminaryGapped alignLeft alignRight


instance Arbitrary BaseChar where
    arbitrary = fmap fromList $ listOf $ choose (0, maxBound :: Int64)
