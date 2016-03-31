module Bio.Sequence.Coded.Test 
  ( testSuite
  ) where

import Bio.Sequence.Coded
import Data.Bits
import Data.BitVector (BitVector)
import Data.Monoid    ((<>))
import Data.Vector    (Vector)
import Test.Tasty                 
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testSuite :: TestTree
testSuite = testGroup "Custom Bits instances" [testVectorBits, testCodeSequenceInstance]

testVectorBits :: TestTree
testVectorBits = testGroup "Properties of instance Bits b => Bits (Vector b)" 
        [ testZeroBitProperties         zeroBitsDynamic "dynamic"
        , testBitConstructionProperties zeroBitsDynamic "dynamic"
        ]
    where
        zeroBitsDynamic = zeroBits :: EncodedSeq

testZeroBitProperties :: Bits b => b -> String -> TestTree
testZeroBitProperties z label = testGroup ("zeroBit properties (" <> label <> ")") 
        [ zeroBitsID 
        , setBitBitEquivalency
        , zeroBitIsAllZeroes
        , popCountZeroBitIs0
        ]
    where
        zeroBitsID :: TestTree
        zeroBitsID = testProperty "clearBit zeroBits n == zeroBits" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  clearBit z i == z  
        setBitBitEquivalency :: TestTree
        setBitBitEquivalency = testProperty "setBit zeroBits n == bit n" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  setBit z i == bit i

        zeroBitIsAllZeroes :: TestTree
        zeroBitIsAllZeroes = testProperty "testBit zeroBits n == False" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  testBit z i == False

        popCountZeroBitIs0 :: TestTree
        popCountZeroBitIs0 = testCase "popCount zeroBits == 0" . assert $ popCount z == 0

testBitConstructionProperties :: Bits b => b -> String -> TestTree
testBitConstructionProperties z label = testGroup ("Bit toggling properties (" <> label <> ")") 
        [ setBitTestBit
        , bitClearBit
        ]
    where
        setBitTestBit :: TestTree
        setBitTestBit = testProperty "testBit (setBit zeroBits i) i == True" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  testBit (setBit z i) i == True
        bitClearBit :: TestTree
        bitClearBit = testProperty "clearBit (bit i) i == zeroBits" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  clearBit (bit i) i == z

testCodeSequenceInstance :: TestTree
testCodeSequenceInstance = testGroup "Properties of instance CodedSequence EncodedSeq" 
        [
        ]


