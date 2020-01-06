{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}

module Bio.Character.Encodable.Static.Test
  ( testSuite
  ) where

import           Bio.Character
import           Data.Alphabet
import           Data.Bits
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Key              ((!))
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Set              (Set)
import qualified Data.Set              as Set (fromList, intersection, union)
import           Data.Vector           (Vector, fromList)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding ((.&.))


testSuite :: TestTree
testSuite = testGroup "Static Character Tests"
    [ bitsTests
    , finiteBitsTests
    , monoFoldableProperties
    , monoFunctorProperties
    , monoTraversableProperties
    , orderingProperties
    , datastructureTests
    ]


bitsTests :: TestTree
bitsTests = testGroup "Bits instance properties"
    [ testProperty "∀ n ≥ 0, clearBit zeroBits n === zeroBits" zeroBitsAndClearBit
    , testProperty "∀ n ≥ 0, setBit   zeroBits n === bit n" zeroBitsAndSetBit
    , testProperty "∀ n ≥ 0, testBit  zeroBits n === False" zeroBitsAndTestBit
    , testCase     "         popCount zeroBits   === 0" zeroBitsAndPopCount
    , testProperty "(`testBit` i) . complement === not . (`testBit` i)" complementTestBit
    , testProperty "(`setBit` n) === (.|. bit n)" setBitDefinition
    , testProperty "(`clearBit` n) === (.&. complement (bit n))" clearBitDefinition
    , testProperty "(`complementBit` n) === (`xor` bit n)" complementBitDefinition
    , testProperty "(`testBit` n) . (`setBit` n)" testBitAndSetBit
    , testProperty "not  . (`testBit` n) . (`clearBit` n)" testBitAndClearBit
    ]
  where
    zeroBitsAndClearBit :: NonNegative Int -> Property
    zeroBitsAndClearBit (NonNegative n) =
        clearBit (zeroBits :: StaticCharacter) n === zeroBits

    zeroBitsAndSetBit :: NonNegative Int -> Property
    zeroBitsAndSetBit (NonNegative n) =
        setBit   (zeroBits :: StaticCharacter) n === bit n

    zeroBitsAndTestBit :: NonNegative Int -> Property
    zeroBitsAndTestBit (NonNegative n) =
        testBit  (zeroBits :: StaticCharacter) n === False

    zeroBitsAndPopCount :: Assertion
    zeroBitsAndPopCount =
        popCount (zeroBits :: StaticCharacter) @?= 0

    complementTestBit :: Positive Int -> StaticCharacter -> Property
    complementTestBit (Positive i) bm =
        Just i < bitSizeMaybe bm ==>
          ((`testBit` i) . complement) bm === (not . (`testBit` i)) bm

    setBitDefinition :: (NonNegative Int, StaticCharacter) -> Property
    setBitDefinition (NonNegative n, bv) =
        bv `setBit` n === bv .|. bit n

    clearBitDefinition :: (NonNegative Int, StaticCharacter) -> Property
    clearBitDefinition (NonNegative n, bv) =
        Just n < bitSizeMaybe bv ==>
          (bv `clearBit` n === bv .&. complement  (zed .|. bit n))
      where
        zed = bv `xor` bv

    complementBitDefinition :: (NonNegative Int, StaticCharacter) -> Property
    complementBitDefinition (NonNegative n, bv) =
        bv `complementBit` n === bv `xor` bit n

    testBitAndSetBit :: (NonNegative Int, StaticCharacter) -> Bool
    testBitAndSetBit (NonNegative n, bv) =
        ((`testBit` n) . (`setBit` n)) bv

    testBitAndClearBit :: (NonNegative Int, StaticCharacter) -> Bool
    testBitAndClearBit (NonNegative n, bv) =
        (not  . (`testBit` n) . (`clearBit` n)) bv


finiteBitsTests :: TestTree
finiteBitsTests = testGroup "FiniteBits instance consistency"
    [ testProperty "fromEnum . symbolCount === finiteBitSize" finiteBitSizeIsDimension
    , testProperty "length . otoList === finiteBitSize" finiteBitSizeIsBitLength
    , testProperty "length . takeWhile not . otoList === countLeadingZeros" countLeadingZeroAndToBits
    , testProperty "length . takeWhile not . reverse . otoList === countTrailingZeros" countTrailingZeroAndToBits
    ]
  where
    finiteBitSizeIsDimension :: StaticCharacter -> Property
    finiteBitSizeIsDimension x =
      (fromEnum . symbolCount) x === finiteBitSize x

    finiteBitSizeIsBitLength :: StaticCharacter -> Property
    finiteBitSizeIsBitLength x =
      (length . otoList) x === finiteBitSize x

    countLeadingZeroAndToBits :: StaticCharacter -> Property
    countLeadingZeroAndToBits x =
      (length . takeWhile not . otoList) x === countLeadingZeros x

    countTrailingZeroAndToBits :: StaticCharacter -> Property
    countTrailingZeroAndToBits x =
      (length . takeWhile not . reverse . otoList) x === countTrailingZeros x


monoFoldableProperties :: TestTree
monoFoldableProperties = testGroup "Properties of MonoFoldable"
    [ testProperty "ofoldr f z t === appEndo (ofoldMap (Endo . f) t ) z" testFoldrFoldMap
    , testProperty "ofoldl' f z t === appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z" testFoldlFoldMap
    , testProperty "ofoldr f z === ofoldr f z . otoList" testFoldr
    , testProperty "ofoldl' f z === ofoldl' f z . otoList" testFoldl
    , testProperty "ofoldr1Ex f z === ofoldr1Ex f z . otoList" testFoldr1
    , testProperty "ofoldl1Ex' f z === ofoldl1Ex' f z . otoList" testFoldl1
    , testProperty "oall f === getAll . ofoldMap (All . f)" testAll
    , testProperty "oany f === getAny . ofoldMap (Any . f)" testAny
    , testProperty "olength === length . otoList" testLength
    , testProperty "onull === (0 ==) . olength" testNull
    , testProperty "headEx === getFirst . ofoldMap1Ex First" testHead
    , testProperty "lastEx === getLast . ofoldMap1Ex Last" testTail
    , testProperty "oelem e /== onotElem e" testInclusionConsistency
    ]
  where
    testFoldrFoldMap :: (Blind (Bool -> Word -> Word), Word, StaticCharacter) -> Property
    testFoldrFoldMap (Blind f, z, bv) =
        ofoldr f z bv === appEndo (ofoldMap (Endo . f) bv) z

    testFoldlFoldMap :: (Blind (Word -> Bool -> Word), Word, StaticCharacter) -> Property
    testFoldlFoldMap (Blind f, z, bv) =
        ofoldl' f z bv === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bv)) z

    testFoldr :: (Blind (Bool -> Word -> Word), Word, StaticCharacter) -> Property
    testFoldr (Blind f, z, bv) =
        ofoldr f z bv === (ofoldr f z . otoList) bv

    testFoldl :: (Blind (Word -> Bool -> Word), Word, StaticCharacter) -> Property
    testFoldl (Blind f, z, bv) =
        ofoldl' f z bv === (ofoldl' f z . otoList) bv

    testFoldr1 :: (Blind (Bool -> Bool -> Bool), StaticCharacter) -> Property
    testFoldr1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldr1Ex f bv === (ofoldr1Ex f . otoList) bv

    testFoldl1 :: (Blind (Bool -> Bool -> Bool), StaticCharacter) -> Property
    testFoldl1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldl1Ex' f bv === (ofoldl1Ex' f . otoList) bv

    testAll :: (Blind (Bool -> Bool), StaticCharacter) -> Property
    testAll (Blind f, bv) =
        oall f bv === (getAll . ofoldMap (All . f)) bv

    testAny :: (Blind (Bool -> Bool), StaticCharacter) -> Property
    testAny (Blind f, bv) =
        oany f bv === (getAny . ofoldMap (Any . f)) bv

    testLength :: StaticCharacter -> Property
    testLength bv =
        olength bv === (length . otoList) bv

    testNull :: StaticCharacter -> Property
    testNull bv =
        onull bv === ((0 ==) . olength) bv

    testHead :: StaticCharacter -> Property
    testHead bv =
        (not . onull) bv ==> headEx bv === (getFirst . ofoldMap1Ex First) bv

    testTail :: StaticCharacter -> Property
    testTail bv =
        (not . onull) bv ==> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Bool, StaticCharacter) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properties of a MonoFunctor"
    [ testProperty "omap id === id" omapId
    , testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: StaticCharacter -> Property
    omapId bm = omap id bm === id bm

    omapComposition
      :: Blind (Bool -> Bool)
      -> Blind (Bool -> Bool)
      -> StaticCharacter
      -> Property
    omapComposition (Blind f) (Blind g) bm =
        (omap f . omap g) bm === omap (f . g) bm


monoTraversableProperties :: TestTree
monoTraversableProperties = testGroup "Properties of MonoTraversable"
    [ testProperty "t . otraverse f === otraverse (t . f)" testNaturality
    , testProperty "otraverse Identity === Identity" testIdentity
    , testProperty "otraverse (Compose . fmap g . f) === Compose . fmap (otraverse g) . otraverse f" testComposition
    ]
  where
    testNaturality :: (Blind (Bool -> [Bool]), StaticCharacter) -> Property
    testNaturality (Blind f, bv) =
        (headMay . otraverse f) bv === otraverse (headMay . f) bv

    testIdentity :: StaticCharacter -> Property
    testIdentity bv =
        otraverse Identity bv === Identity bv

    testComposition :: (Blind (Bool -> Either Word Bool), Blind (Bool -> Maybe Bool), StaticCharacter) -> Property
    testComposition (Blind f, Blind g, bv) =
        otraverse (Compose . fmap g . f) bv === (Compose . fmap (otraverse g) . otraverse f) bv


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of an Ordering"
    [ testProperty "ordering preserves symmetry"  symmetry
    , testProperty "ordering is transitive (total)" transitivity
    ]
  where
    symmetry :: StaticCharacter -> StaticCharacter -> Bool
    symmetry lhs rhs =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity
      :: StaticCharacter
      -> StaticCharacter
      -> StaticCharacter
      -> Property
    transitivity a b c = caseOne .||. caseTwo
      where
        caseOne = (a <= b && b <= c) ==> a <= c
        caseTwo = (a >= b && b >= c) ==> a >= c


datastructureTests :: TestTree
datastructureTests = testGroup "Static Character data structure tests"
    [ testEncodableStaticCharacterInstanceDynamicCharacter
    ]


{- LAWS:
 - decodeElement alphabet . encodeChar alphabet . toList == id
 - encodeChar alphabet [alphabet !! i] == bit i
 - encodeChar alphabet alphabet == complement zeroBits
 - decodeElement alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.union` toList ys)
 - decodeElement alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) == toList alphabet `Data.List.intersect` (toList xs `Data.List.intersect` toList ys)
 -}
testEncodableStaticCharacterInstanceDynamicCharacter :: TestTree
testEncodableStaticCharacterInstanceDynamicCharacter = testGroup "DynamicCharacter instance of EncodableDynamicCharacter" [testLaws]
  where
    encodeChar' :: Alphabet String -> NonEmpty String -> StaticCharacter
    encodeChar' = encodeElement
    testLaws = testGroup "EncodableDynamicCharacter Laws"
        [ encodeDecodeIdentity
        , singleBitConstruction
        , totalBitConstruction
        , logicalOrIsomorphismWithSetUnion
        , logicalAndIsomorphismWithSetIntersection
        ]
      where
        encodeDecodeIdentity = testProperty "Set.fromList . decodeElement alphabet . encodeChar alphabet . Set.fromList . toList == Set.fromList . toList" f
          where
            f :: AlphabetAndSingleAmbiguityGroup -> Property
            f alphabetAndAmbiguityGroup = lhs ambiguityGroup === rhs ambiguityGroup
              where
                lhs = Set.fromList . toList . decodeElement alphabet . encodeChar' alphabet . NE.fromList . toList . Set.fromList . toList
                rhs = Set.fromList . toList
                (alphabet, ambiguityGroup) = getAlphabetAndSingleAmbiguityGroup alphabetAndAmbiguityGroup

        singleBitConstruction = testProperty "encodeChar alphabet [alphabet ! i] == bit i" f
          where
            f :: Alphabet String -> NonNegative Int -> Property
            f alphabet (NonNegative n) = countLeadingZeros (encodeChar' alphabet (pure $ alphabet ! i)) === countLeadingZeros (bit i :: StaticCharacter)
              where
                i = n `mod` length alphabet

        totalBitConstruction = testProperty "encodeChar alphabet alphabet == complement (bit (length alphabet - 1) `clearBit` (bit (length alphabet - 1))" f
          where
            f :: Alphabet String -> Property
            f alphabet = encodeChar' alphabet allSymbols === e
              where
                allSymbols = NE.fromList $ toList alphabet
                e = complement $ bit i `clearBit` i
                i = length alphabet - 1

        logicalOrIsomorphismWithSetUnion = testProperty "Set.fromList (decodeElement alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys)) == Set.fromList (toList alphabet) `Set.intersect` (toList xs `Set.union` toList ys)" f
          where
            f :: AlphabetAndTwoAmbiguityGroups -> Property
            f input = lhs === rhs
              where
                lhs = Set.fromList . toList $ decodeElement alphabet (encodeChar' alphabet (fromFoldable sxs) .|. encodeChar' alphabet (fromFoldable sys))
                rhs = sxs `Set.union` sys
                (alphabet, sxs,sys) = gatherAlphabetAndAmbiguitySets input

        logicalAndIsomorphismWithSetIntersection = testProperty "Set.fromList (decodeElement alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys)) == Set.fromList (toList alphabet) `Set.intersect` (toList xs `Set.intersection` toList ys)" f
          where
            f :: AlphabetAndTwoAmbiguityGroups -> Property
            f input =
                (not . null) rhs ==> lhs === rhs
              where
                rhs   = sxs `Set.intersection` sys
                lhs   = Set.fromList . toList $ decodeElement alphabet anded
                anded = xs' .&. ys'
                xs'   = encodeChar' alphabet (fromFoldable sxs)
                ys'   = encodeChar' alphabet (fromFoldable sys)
                (alphabet, sxs, sys) = gatherAlphabetAndAmbiguitySets input


gatherAlphabetAndAmbiguitySets :: AlphabetAndTwoAmbiguityGroups -> (Alphabet String, Set String, Set String)
gatherAlphabetAndAmbiguitySets input = (alphabet, Set.fromList $ toList xs, Set.fromList $ toList ys)
  where
    (alphabet, xs, ys) = getAlphabetAndTwoAmbiguityGroups input


-- Types for supporting 'Arbitrary' construction

type ParsedChar' = Vector (NonEmptyList (NonEmptyList Char))


newtype ParsedCharacterWithAlphabet
      = ParsedCharacterWithAlphabet (ParsedChar', Alphabet String)
      deriving stock (Eq, Show)


instance Arbitrary ParsedCharacterWithAlphabet where
    arbitrary = do
        alphabet  <- arbitrary :: Gen (Alphabet String)
        vectorVal <- fmap (fmap (NonEmpty . (:[]) . NonEmpty) . fromList) . listOf1 . elements . toList $ alphabet
        pure $ ParsedCharacterWithAlphabet (vectorVal, alphabet)


newtype AlphabetAndSingleAmbiguityGroup
      = AlphabetAndSingleAmbiguityGroup
      { getAlphabetAndSingleAmbiguityGroup :: (Alphabet String, NonEmpty String)
      } deriving stock (Eq, Show)


instance Arbitrary AlphabetAndSingleAmbiguityGroup where
  arbitrary = do
    (alphabet, x :| _) <- alphabetAndAmbiguityGroups 1
    pure $ AlphabetAndSingleAmbiguityGroup (alphabet, x)


newtype AlphabetAndTwoAmbiguityGroups
      = AlphabetAndTwoAmbiguityGroups
      { getAlphabetAndTwoAmbiguityGroups :: (Alphabet String, NonEmpty String, NonEmpty String)
      } deriving stock (Eq, Show)


instance Arbitrary AlphabetAndTwoAmbiguityGroups where
  arbitrary = do
    (alphabet, xs) <- alphabetAndAmbiguityGroups 2
    pure $ case xs of
             x:|[]  -> AlphabetAndTwoAmbiguityGroups (alphabet, x, x)
             x:|y:_ -> AlphabetAndTwoAmbiguityGroups (alphabet, x, y)

newtype AlphabetAndCharacter
      = AlphabetAndCharacter (Alphabet String, NonEmpty (NonEmpty String))
      deriving stock (Eq, Show)


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
