{-# LANGUAGE FlexibleInstances #-}

module Bio.Character.Encodable.Dynamic.Test
  ( testSuite
  ) where

import           Bio.Character.Encodable.Dynamic
import           Data.Alphabet
import           Data.Bits
import           Data.Foldable
import           Data.Key                  ((!))
import           Data.List.NonEmpty        (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Set                  (Set)
import qualified Data.Set           as Set (fromList,intersection,union)
import           Data.Vector               (Vector, fromList)
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((.&.))

--import Debug.Trace (trace)

testSuite :: TestTree
testSuite = testGroup "Custom Bits instances"
        [ testEncodableStaticCharacterInstanceBitVector
        , testEncodableDynamicCharacterInstanceDynamicChar
--        , testVectorBits
        ]


{- LAWS:
 - decodeElement alphabet . encodeChar alphabet . toList == id
 - encodeChar alphabet [alphabet !! i] == bit i
 - encodeChar alphabet alphabet == complement zeroBits
 - decodeElement alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)
 - decodeElement alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)
 -}
testEncodableStaticCharacterInstanceBitVector :: TestTree
testEncodableStaticCharacterInstanceBitVector = testGroup "BitVector instance of EncodableDynamicCharacter" [testLaws]
  where
    encodeChar' :: Alphabet String -> NonEmpty String -> DynamicCharacterElement
    encodeChar' = encodeElement
    testLaws = testGroup "EncodableDynamicChar Laws"
             [ encodeDecodeIdentity
             , singleBitConstruction
             , totalBitConstruction
             , logicalOrIsomorphismWithSetUnion
             , logicalAndIsomorphismWithSetIntersection
             ]
      where
        encodeDecodeIdentity = testProperty "Set.fromList . decodeElement alphabet . encodeChar alphabet . Set.fromList . toList == Set.fromList . toList" f
          where
            f :: AlphabetAndSingleAmbiguityGroup -> Bool
            f alphabetAndAmbiguityGroup = lhs ambiguityGroup == rhs ambiguityGroup
              where
                lhs = Set.fromList . toList . decodeElement alphabet . encodeChar' alphabet . NE.fromList . toList . Set.fromList . toList
                rhs = Set.fromList . toList
                (alphabet, ambiguityGroup) = getAlphabetAndSingleAmbiguityGroup alphabetAndAmbiguityGroup

        singleBitConstruction = testProperty "encodeChar alphabet [alphabet ! i] == bit i" f
          where
            f :: Alphabet String -> NonNegative Int -> Bool
            f alphabet (NonNegative n) = encodeChar' alphabet (pure $ alphabet ! i) == bit i
              where
                i = n `mod` length alphabet

        totalBitConstruction = testProperty "encodeChar alphabet alphabet == complement (bit (length alphabet - 1) `clearBit` (bit (length alphabet - 1))" f
          where
            f :: Alphabet String -> Bool
            f alphabet = encodeChar' alphabet allSymbols == e
              where
                allSymbols = NE.fromList $ toList alphabet
                e = complement $ bit i `clearBit` i
                i = length alphabet - 1

        logicalOrIsomorphismWithSetUnion = testProperty "Set.fromList (decodeElement alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys)) == Set.fromList (toList alphabet) `Set.intersect` (toList xs `Set.union` toList ys)" f
          where
            f :: AlphabetAndTwoAmbiguityGroups -> Bool
            f input = lhs == rhs
              where
                lhs = Set.fromList . toList $ decodeElement alphabet (encodeChar' alphabet (fromFoldable sxs) .|. encodeChar' alphabet (fromFoldable sys))
                rhs = sxs `Set.union` sys
                (alphabet, sxs,sys) = gatherAlphabetAndAmbiguitySets input

        logicalAndIsomorphismWithSetIntersection = testProperty "Set.fromList (decodeElement alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys)) == Set.fromList (toList alphabet) `Set.intersect` (toList xs `Set.intersection` toList ys)" f
          where
            f :: AlphabetAndTwoAmbiguityGroups -> Bool
            f input
              | zeroBits == anded = null rhs
              | otherwise         = lhs == rhs
              where
                lhs   = Set.fromList . toList $ decodeElement alphabet anded
                anded = encodeChar' alphabet (fromFoldable sxs) .&. encodeChar' alphabet (fromFoldable sys)
                rhs   = sxs `Set.intersection` sys
                (alphabet, sxs, sys) = gatherAlphabetAndAmbiguitySets input


gatherAlphabetAndAmbiguitySets :: AlphabetAndTwoAmbiguityGroups -> (Alphabet String, Set String, Set String)
gatherAlphabetAndAmbiguitySets input = (alphabet, Set.fromList $ toList xs, Set.fromList $ toList ys)
  where
    (alphabet, xs, ys) = getAlphabetAndTwoAmbiguityGroups input


{- LAWS:
 - decodeMany alphabet . encodeMany alphabet == fmap toList . toList
 -}
testEncodableDynamicCharacterInstanceDynamicChar :: TestTree
testEncodableDynamicCharacterInstanceDynamicChar = testGroup "DynamicChar instance of EncodableDynamicCharacter" [testLaws]
  where
    testLaws = testGroup "EncodableDynamicChar Laws"
             [ encodeDecodeIdentity
             ]
      where
        encodeDecodeIdentity = testProperty "decodeDynamic alphabet . encodeDynamic alphabet == fmap toList . toList" f
          where
            f :: AlphabetAndCharacter -> Bool
            f alphabetAndDynamicChar = lhs dynamicChar == rhs dynamicChar
              where
                enc :: NonEmpty (AmbiguityGroup String) -> DynamicChar
                enc = encodeStream alphabet
                lhs = fmap (Set.fromList . toList) . toList . decodeStream alphabet . enc
                rhs = fmap (Set.fromList . toList) . toList
                (alphabet, dynamicChar) = getAlphabetAndCharacter alphabetAndDynamicChar


-- Types for supporting 'Arbitrary' construction

type ParsedChar' = Vector (NonEmptyList (NonEmptyList Char))


newtype ParsedCharacterWithAlphabet
      = ParsedCharacterWithAlphabet (ParsedChar', Alphabet String)
      deriving (Eq, Show)


instance Arbitrary ParsedCharacterWithAlphabet where
    arbitrary = do
        alphabet  <- arbitrary :: Gen (Alphabet String)
        vectorVal <- fmap (fmap (NonEmpty . (:[]) . NonEmpty) . fromList) . listOf1 . elements . toList $ alphabet
        pure $ ParsedCharacterWithAlphabet (vectorVal, alphabet)


newtype AlphabetAndSingleAmbiguityGroup
      = AlphabetAndSingleAmbiguityGroup
      { getAlphabetAndSingleAmbiguityGroup :: (Alphabet String, NonEmpty String)
      } deriving (Eq, Show)


instance Arbitrary AlphabetAndSingleAmbiguityGroup where
  arbitrary = do
    (alphabet, x :| _) <- alphabetAndAmbiguityGroups 1
    pure $ AlphabetAndSingleAmbiguityGroup (alphabet, x)


newtype AlphabetAndTwoAmbiguityGroups
      = AlphabetAndTwoAmbiguityGroups
      { getAlphabetAndTwoAmbiguityGroups :: (Alphabet String, NonEmpty String, NonEmpty String)
      } deriving (Eq, Show)


instance Arbitrary AlphabetAndTwoAmbiguityGroups where
  arbitrary = do
    (alphabet, xs) <- alphabetAndAmbiguityGroups 2
    pure $ case xs of
             x:|[]   -> AlphabetAndTwoAmbiguityGroups (alphabet, x, x)
             x:|y:[] -> AlphabetAndTwoAmbiguityGroups (alphabet, x, y)


newtype AlphabetAndCharacter
      = AlphabetAndCharacter
      { getAlphabetAndCharacter :: (Alphabet String, NonEmpty (NonEmpty String))
      } deriving (Eq, Show)


instance Arbitrary AlphabetAndCharacter where
  arbitrary = do
    alphabet           <- arbitrary :: Gen (Alphabet String)
    let ambiguityGroup =  fmap NE.fromList . listOf1 . elements $ toList alphabet
    dynamicChar        <- NE.fromList <$> listOf1 ambiguityGroup
    pure $ AlphabetAndCharacter (alphabet, dynamicChar)


alphabetAndAmbiguityGroups :: Int -> Gen (Alphabet String, NonEmpty (NonEmpty String))
alphabetAndAmbiguityGroups n = do
   alphabet           <- arbitrary :: Gen (Alphabet String)
   let ambiguityGroup =  fmap NE.fromList . listOf1 . elements $ toList alphabet -- list can be empty, can have duplicates!
   ambiguityGroups    <- NE.fromList <$> vectorOf n ambiguityGroup
   pure (fromSymbols alphabet, ambiguityGroups)


fromFoldable :: Set a -> NonEmpty a
fromFoldable = NE.fromList . toList




-- TODO: (think) I think that DynamicChar shouldn't be an instance of Bits, just the subcharacters should be...
{--
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
--}

