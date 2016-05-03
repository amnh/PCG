{-# LANGUAGE FlexibleInstances #-}

module Bio.Character.Dynamic.Coded.Test
  ( testSuite
  ) where

import Bio.Character.Dynamic.Coded
import Bio.Character.Dynamic.Parsed
import Data.Bits
import Data.BitVector (BitVector, toBits, width)
import Data.Monoid    ((<>))
import Data.Vector    (Vector, fromList)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

testSuite :: TestTree
testSuite = testGroup "Custom Bits instances" [testVectorBits, testCodedSequenceInstance]

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
                      in  not $ testBit z i

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
                      in  testBit (setBit z i) i
        bitClearBit :: TestTree
        bitClearBit = testProperty "clearBit (bit i) i == zeroBits" f
            where
                f :: NonNegative Int -> Bool
                f n = let i = getNonNegative n
                      in  clearBit (bit i) i == z

testCodedSequenceInstance :: TestTree
testCodedSequenceInstance = testGroup "Properties of instance CodedSequence EncodedSeq"
        [ --decodeOverAlphabet
        encodeOverAlphabetTest
        --, filterGaps
        --, grabSubChar
        --, isEmpty
        --, numChars
        ]
encodeOverAlphabetTest :: TestTree
encodeOverAlphabetTest = testGroup "encodeOverAlphabet"
    [
    ]

encodeOverAlphabetTest :: TestTree
encodeOverAlphabetTest = testGroup "encodeOverAlphabet"
    [ testWidth
    , testValue
    ]
    where
        testWidth = testProperty "Make sure width of encoded seq == len(alphabet) * len(parsed seq)." f
            where
                f :: (ParsedSeq', Alphabet) -> Bool
                f (inSeq, alph) = width (encodeOverAlphabet (getParsedSeq inSeq) alph :: EncodedSeq) == length alph * (length $ getParsedSeq inSeq)
        testValue = testProperty "Make sure encoded value matches position in alphabet." f
            where
                f :: (ParsedSeq', Alphabet) -> Bool
                f (inSeq, alph) = toBits (encodeOverAlphabet (getParsedSeq inSeq) alph :: EncodedSeq) == fmap (alph `elem`) inSeq

type ParsedSeq' = Vector (NonEmptyList (NonEmptyList Char))

getParsedSeq :: ParsedSeq' -> ParsedSeq
getParsedSeq = fmap (fmap getNonEmpty . getNonEmpty)

--decodeOverAlphabetTest :: TestTree
--decodeOverAlphabetTest = testProperty "decodeOverAlphabet" f
--    where
--        f :: CodedSequence s -> Alphabet -> ParsedSeq
--        f inSeq alph =

instance (Arbitrary a) => Arbitrary (Vector a) where
    arbitrary = do
        i <- arbitrary :: Gen Int
        fmap fromList . sequence . fmap (const arbitrary) . take i $ repeat ()

instance Arbitrary (ParsedSeq', Alphabet) where
    arbitrary = do
        alphabet <- (fmap (:[]) . getNonEmpty) <$> (arbitrary :: (Gen (NonEmptyList Char)))
        vector   <- fmap (fmap (NonEmpty . (:[]) . NonEmpty) . fromList) . listOf1 $ elements alphabet
        pure (vector, alphabet)