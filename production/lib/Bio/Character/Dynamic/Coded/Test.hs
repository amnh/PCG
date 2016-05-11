{-# LANGUAGE FlexibleInstances #-}

module Bio.Character.Dynamic.Coded.Test
  ( testSuite
  ) where

import Bio.Character.Dynamic.Coded
import Bio.Character.Parsed
import Data.Alphabet
import Data.Bits
import Data.BitVector (BitVector, toBits, width)
import Data.Foldable
import qualified Data.Set as Set (fromList)
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
        zeroBitsDynamic = zeroBits :: DynamicChar

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
    [ testAlphabetLen
    --, testValue
    ]
    where
        testAlphabetLen = testProperty "Make sure alphabet length of encoded dynamic char == len(alphabet)." f
            where
                f :: (ParsedChar', Alphabet) -> Bool
                f (inChar, alph) = getAlphLen (encodeOverAlphabet alph (getParsedChar inChar) :: DynamicChar) == length alph
        {-testValue = testProperty "Make sure encoded value matches position in alphabet." f
            where
                -- for each ambiguity group in inChar, map over the alphabet determining whether each alphabet state exists in the ambiguity group
                f :: (ParsedChar', Alphabet) -> Bool
                f (inChar, alph) = toBits controlChar == (fmap (\c -> elem c alph) $ toList charToTest)
                    where 
                        charToTest  = getParsedChar inChar
                        controlChar = (encodeOverAlphabet alph charToTest :: DynamicChar)-}


type DynamicChar' = Vector

type ParsedChar' = Vector (NonEmptyList (NonEmptyList Char))

getParsedChar :: ParsedChar' -> ParsedChar
getParsedChar = fmap (fmap getNonEmpty . getNonEmpty)

--decodeOverAlphabetTest :: TestTree
--decodeOverAlphabetTest = testProperty "decodeOverAlphabet" f
--    where
--        f :: CodedSequence s -> Alphabet -> ParsedChar
--        f inChar alph =

instance Arbitrary (ParsedChar', Alphabet) where
    arbitrary = do
        alphabet <- (fmap (:[]) . getNonEmpty) <$> (arbitrary :: (Gen (NonEmptyList Char)))
        vector   <- fmap (fmap (NonEmpty . (:[]) . NonEmpty) . fromList) . listOf1 $ elements alphabet
        pure (vector, fromList alphabet)


{- LAWS:
 - decodeChar alphabet . encodeChar alphabet . toList == id
 - encodeChar alphabet [alphabet !! i] == bit i
 - encodeChar alphabet alphabet == compliment zeroBits
 - decodeChar alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)
 - decodeChar alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)
 - finiteBitSize . encodeChar alphabet == const (length alphabet)
 -}
testEncodableStaticCharacterInstanceBitVector :: TestTree
testEncodableStaticCharacterInstanceBitVector = testGroup "BitVector instance of EncodableDynamicCharacter" [testLaws]
  where
    testLaws = testGroup "EncodableDynamicChar Laws" [encodeDecodeIdentity]
      where
        encodeDecodeIdentity = testProperty "Set.fromList . decodeChar alphabet . encodeChar alphabet . Set.fromList . toList \n== Set.fromList . toList" f
          where
            f :: (Alphabet' String, [String]) -> Bool
            f (alphabet, ambiguityGroup) = lhs ambiguityGroup == rhs ambiguityGroup
              where
                enc :: (Foldable t) => t String -> BitVector 
                enc = encodeChar alphabet
                lhs = Set.fromList . decodeChar alphabet . enc . Set.fromList . toList
                rhs = Set.fromList . toList


instance Arbitrary (Alphabet' String, [String]) where
  arbitrary = do
    (alphabet,[x]) <- alphabetAndAmbiguityGroups 1
    pure (alphabet, x)

instance Arbitrary (Alphabet' String, [String], [String]) where
  arbitrary = do
    (alphabet,[x,y]) <- alphabetAndAmbiguityGroups 2
    pure (alphabet, x, y)

alphabetAndAmbiguityGroups :: Int -> Gen (Alphabet' String, [[String]])
alphabetAndAmbiguityGroups n = do
   alphabet        <- constructAlphabet . getNonEmpty <$> (arbitrary :: Gen (NonEmptyList String))
   let ambiguityGroup = listOf . elements $ toList alphabet -- list can be empty, can have duplicates!
   ambiguityGroups <- vectorOf n ambiguityGroup
   pure (constructAlphabet alphabet, ambiguityGroups)
                                
