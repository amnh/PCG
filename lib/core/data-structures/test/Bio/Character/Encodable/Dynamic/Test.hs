------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Test
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Bio.Character.Encodable.Dynamic.Test
  ( testSuite
  ) where

import Bio.Character
import Data.Bits
import Data.MonoTraversable
import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))


-- |
-- The test-suite for the 'DynamicCharacter' data type.
testSuite :: TestTree
testSuite = testGroup "Dynamic Character tests"
    [ ambiguityGroupTests
    , dynamicCharacterElementTests
    , dynamicCharacterTests
    ]


dynamicCharacterTests :: TestTree
dynamicCharacterTests = testGroup "Dynamic Character tests"
    [ monoFunctorPropertiesGen  @DynamicCharacter
    , monoFoldablePropertiesGen @DynamicCharacter
    , orderingLaws              @AmbiguityGroup
--    , datastructureTests
    ]


ambiguityGroupTests :: TestTree
ambiguityGroupTests = testGroup "Ambiguity Group tests"
    [ bitsProperties            @AmbiguityGroup
    , finiteBitsProperties      @AmbiguityGroup
    , monoFunctorPropertiesGen  @AmbiguityGroup
    , monoFoldablePropertiesGen @AmbiguityGroup
    , orderingLaws              @AmbiguityGroup
    ]


dynamicCharacterElementTests :: TestTree
dynamicCharacterElementTests = testGroup "Dynamic Character Element tests"
    [ --elementBitsTests
--    , elementFiniteBitsTests
--    , monoFoldablePropertiesGen @DynamicCharacterElement
--    , elementMonoFunctorProperties
      orderingLaws              @AmbiguityGroup
    ]


bitsProperties
  :: forall f.
     ( Arbitrary f
     , FiniteBits f
     , EncodedAmbiguityGroupContainer f
     , MonoFunctor f
     , Show f
     , Element f ~ Bool
     )
  => TestTree
bitsProperties = testGroup "Properties of Bits"
    [ testProperty "∀ i ≥ 0, clearBit zeroBits i === zeroBits" zeroBitsAndClearBit
    , testProperty "∀ i ≥ 0, setBit   zeroBits i === bit i" zeroBitsAndSetBit
    , testProperty "∀ i ≥ 0, testBit  zeroBits i === False" zeroBitsAndTestBit
    , testCase     "         popCount zeroBits   === 0" zeroBitsAndPopCount
    , testProperty "complement === omap not" complementOmapNot
    , testProperty "(`setBit` i) === (.|. bit i)" setBitDefinition
    , testProperty "(`clearBit` i) === (.&. complement (bit i))" clearBitDefinition
    , testProperty "(`complementBit` i) === (`xor` bit i)" complementBitDefinition
    , testProperty "(`testBit` i) . (`setBit` n)" testBitAndSetBit
    , testProperty "not  . (`testBit` i) . (`clearBit` i)" testBitAndClearBit
    , testProperty "(`shiftL`  i) === (`shift`   i)" leftShiftPositiveShift
    , testProperty "(`shiftR`  i) === (`shift`  -i)" rightShiftNegativeShift
    , testProperty "(`rotateL` i) === (`rotate`  i)" leftRotatePositiveRotate
    , testProperty "(`rotateR` i) === (`rotate` -i)" rightRotateNegativeRotate
    , testProperty "(`rotateR` i) . (`rotateL` i) === id" leftRightRotateIdentity
    , testProperty "(`rotateL` i) . (`rotateR` i) === id" rightLeftRotateIdentity
    ]
  where
    zeroBitsAndClearBit :: NonNegative Int -> Property
    zeroBitsAndClearBit (NonNegative i) =
        clearBit (zeroBits :: f) i === zeroBits

    zeroBitsAndSetBit :: NonNegative Int -> Property
    zeroBitsAndSetBit (NonNegative i) =
        setBit   (zeroBits :: f) i === bit i

    zeroBitsAndTestBit :: NonNegative Int -> Property
    zeroBitsAndTestBit (NonNegative i) =
        testBit  (zeroBits :: f) i === False

    zeroBitsAndPopCount :: Assertion
    zeroBitsAndPopCount =
        popCount (zeroBits :: f) @?= 0

    complementOmapNot :: f -> Property
    complementOmapNot bv =
        complement bv === omap not bv

    setBitDefinition :: NonNegative Int -> f -> Property
    setBitDefinition (NonNegative i) bv =
        bv `setBit` i === bv .|. bit i

    clearBitDefinition :: NonNegative Int -> f -> Property
    clearBitDefinition (NonNegative i) bv =
        i < (fromEnum . symbolCount) bv -=>
          (bv `clearBit` i === bv .&. complement  (zed .|. bit i))
      where
        zed = bv `xor` bv

    complementBitDefinition :: NonNegative Int -> f -> Property
    complementBitDefinition (NonNegative i) bv =
        bv `complementBit` i === bv `xor` bit i

    testBitAndSetBit :: NonNegative Int -> f -> Bool
    testBitAndSetBit (NonNegative i) =
        (`testBit` i) . (`setBit` i)

    testBitAndClearBit :: NonNegative Int -> f -> Bool
    testBitAndClearBit (NonNegative i) =
        not  . (`testBit` i) . (`clearBit` i)

    leftShiftPositiveShift :: NonNegative Int -> f -> Property
    leftShiftPositiveShift (NonNegative i) bv =
        bv `shiftL` i === bv `shift` i

    rightShiftNegativeShift :: NonNegative Int -> f -> Property
    rightShiftNegativeShift (NonNegative i) bv =
        bv `shiftR` i === bv `shift` (-i)

    leftRotatePositiveRotate :: NonNegative Int -> f -> Property
    leftRotatePositiveRotate (NonNegative i) bv =
        bv `rotateL` i === bv `rotate` i

    rightRotateNegativeRotate :: NonNegative Int -> f -> Property
    rightRotateNegativeRotate (NonNegative i) bv =
        bv `rotateR` i === bv `rotate` (-i)

    leftRightRotateIdentity :: NonNegative Int -> f -> Property
    leftRightRotateIdentity (NonNegative i) bv =
        ((`rotateR` i) . (`rotateL` i)) bv === bv

    rightLeftRotateIdentity :: NonNegative Int -> f -> Property
    rightLeftRotateIdentity (NonNegative i) bv =
        ((`rotateL` i) . (`rotateR` i)) bv === bv


finiteBitsProperties
  :: forall f.
     ( Arbitrary f
     , FiniteBits f
     , EncodedAmbiguityGroupContainer f
     , MonoFoldable f
     , Show f
     , Element f ~ Bool
     )
  => TestTree
finiteBitsProperties = testGroup "Properties of FiniteBits"
    [ testProperty "bitSizeMaybe === Just . finiteBitSize" finiteBitSizeIsBitSizeMaybe
    , testProperty "fromEnum . symbolCount === finiteBitSize" finiteBitSizeIsSymbolCount
    , testProperty "countLeadingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanLeadingZeros
    , testProperty "countTrailingZeros <= finiteBitSize" finiteBitSizeIsGreaterThanTrailingZeros
    , testProperty "length . otoList === finiteBitSize" finiteBitSizeIsBitLength
    , testProperty "length . takeWhile not . otoList === countLeadingZeros" countLeadingZeroAndToBits
    , testProperty "length . takeWhile not . reverse . otoList === countTrailingZeros" countTrailingZeroAndToBits
    ]
  where
    finiteBitSizeIsBitSizeMaybe :: f -> Property
    finiteBitSizeIsBitSizeMaybe bv =
        bitSizeMaybe bv === (Just . finiteBitSize) bv

    finiteBitSizeIsSymbolCount :: f -> Property
    finiteBitSizeIsSymbolCount bv =
        (fromEnum . symbolCount) bv === finiteBitSize bv

    finiteBitSizeIsGreaterThanLeadingZeros :: f -> Bool
    finiteBitSizeIsGreaterThanLeadingZeros bv =
        countLeadingZeros bv <= finiteBitSize bv

    finiteBitSizeIsGreaterThanTrailingZeros :: f -> Bool
    finiteBitSizeIsGreaterThanTrailingZeros bv =
        countTrailingZeros bv <= finiteBitSize bv

    finiteBitSizeIsBitLength :: f -> Property
    finiteBitSizeIsBitLength bv =
        (length . otoList) bv === finiteBitSize bv

    countLeadingZeroAndToBits :: f -> Property
    countLeadingZeroAndToBits bv =
        (length . takeWhile not . otoList) bv === countLeadingZeros bv

    countTrailingZeroAndToBits :: f -> Property
    countTrailingZeroAndToBits bv =
       (length . takeWhile not . reverse . otoList) bv === countTrailingZeros bv


monoFunctorPropertiesGen
  :: forall f.
     ( MonoFunctor f
     , Arbitrary f
     , Arbitrary (Element f)
     , CoArbitrary (Element f)
     , Eq f
     , Show f
     )
  => TestTree
monoFunctorPropertiesGen = testGroup "Properites of a MonoFunctor"
    [ testProperty "omap id === id" omapId
    , testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: f -> Property
    omapId bv =
        omap id bv === bv

    omapComposition :: Blind (Element f -> Element f) -> Blind (Element f -> Element f) -> f -> Property
    omapComposition (Blind f) (Blind g) bv =
        omap (f . g) bv ===  (omap f . omap g) bv


monoFoldablePropertiesGen
  :: forall f.
     ( MonoFoldable f
     , Arbitrary f
     , Arbitrary (Element f)
     , CoArbitrary (Element f)
     , Eq (Element f)
     , Show f
     , Show (Element f)
     )
  => TestTree
monoFoldablePropertiesGen = testGroup "Properties of MonoFoldable"
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
    testFoldrFoldMap :: (Blind (Element f -> Word -> Word), Word, f) -> Property
    testFoldrFoldMap (Blind f, z, bv) =
        ofoldr f z bv === appEndo (ofoldMap (Endo . f) bv) z

    testFoldlFoldMap :: (Blind (Word -> Element f -> Word), Word, f) -> Property
    testFoldlFoldMap (Blind f, z, bv) =
        ofoldl' f z bv === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bv)) z

    testFoldr :: (Blind (Element f -> Word -> Word), Word, f) -> Property
    testFoldr (Blind f, z, bv) =
        ofoldr f z bv === (ofoldr f z . otoList) bv

    testFoldl :: (Blind (Word -> Element f -> Word), Word, f) -> Property
    testFoldl (Blind f, z, bv) =
        ofoldl' f z bv === (ofoldl' f z . otoList) bv

    testFoldr1 :: (Blind (Element f -> Element f -> Element f), f) -> Property
    testFoldr1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldr1Ex f bv === (ofoldr1Ex f . otoList) bv

    testFoldl1 :: (Blind (Element f -> Element f -> Element f), f) -> Property
    testFoldl1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldl1Ex' f bv === (ofoldl1Ex' f . otoList) bv

    testAll :: (Blind (Element f -> Bool), f) -> Property
    testAll (Blind f, bv) =
        oall f bv === (getAll . ofoldMap (All . f)) bv

    testAny :: (Blind (Element f -> Bool), f) -> Property
    testAny (Blind f, bv) =
        oany f bv === (getAny . ofoldMap (Any . f)) bv

    testLength :: f -> Property
    testLength bv =
        olength bv === (length . otoList) bv

    testNull :: f -> Property
    testNull bv =
        onull bv === ((0 ==) . olength) bv

    testHead :: f -> Property
    testHead bv =
        (not . onull) bv ==> headEx bv === (getFirst . ofoldMap1Ex First) bv

    testTail :: f -> Property
    testTail bv =
        (not . onull) bv ==> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Element f, f) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


orderingLaws
  :: forall a.
     ( Arbitrary a
     , Ord a
     , Show a
     )
  => TestTree
orderingLaws = testGroup "Ordering Laws"
    [ testLaw symmetry       "Symmetry"       "x >= y ==> y <= x"
    , testLaw transitivity1 "Transitive I"  "x < y && y < z ==> x < z"
    , testLaw transitivity2 "Transitive II" "x > y && y > z ==> x > z"
    ]
  where
    symmetry :: a -> a -> Bool
    symmetry lhs rhs =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity1 :: a -> a -> a -> Property
    transitivity1 x y z =
        (x < y && y < z) ==> x < z

    transitivity2 :: a -> a -> a -> Property
    transitivity2 x y z =
        (x > y && y > z) ==> x > z



{-
datastructureTests :: TestTree
datastructureTests = testGroup "Dynamic Character data structure tests"
    [ testEncodableStaticCharacterInstanceDynamicCharacter
--    , testEncodableDynamicCharacterInstanceDynamicCharacter
--    , testVectorBits
    ]
-}


{-
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
    encodeChar' :: Alphabet String -> NonEmpty String -> DynamicCharacterElement
    encodeChar' = encodeElement
    testLaws = testGroup "EncodableDynamicCharacter Laws"
             [ encodeDecodeIdentity
             , singleBitConstruction
             , totalBitConstruction
             , logicalOrIsomorphismWithSetUnion
             , logicalAndIsomorphismWithSetIntersection
             ]
      where
        encodeDecodeIdentity = testProperty "decodeElement alphabet . encodeChar alphabet === id" f
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
            f alphabet (NonNegative n) = countLeadingZeros (encodeChar' alphabet (pure $ alphabet ! i)) === countLeadingZeros (bit i :: DynamicCharacterElement)
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

        logicalOrIsomorphismWithSetUnion = testProperty "decodeElement alphabet (encodeChar alphabet xs .|. encodeChar alphabet ys) === alphabet ∩ (xs ∪ ys)" f
          where
            f :: AlphabetAndTwoAmbiguityGroups -> Property
            f input = lhs === rhs
              where
                lhs = Set.fromList . toList $ decodeElement alphabet (encodeChar' alphabet (fromFoldable sxs) .|. encodeChar' alphabet (fromFoldable sys))
                rhs = sxs `Set.union` sys
                (alphabet, sxs,sys) = gatherAlphabetAndAmbiguitySets input

        logicalAndIsomorphismWithSetIntersection = testProperty "decodeElement alphabet (encodeChar alphabet xs .&. encodeChar alphabet ys) === alphabet ∩ (xs ∩ ys)" f
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

{- LAWS:
 - decodeMany alphabet . encodeMany alphabet == fmap toList . toList
 -}
testEncodableDynamicCharacterInstanceDynamicCharacter :: TestTree
testEncodableDynamicCharacterInstanceDynamicCharacter = testGroup "DynamicCharacter instance of EncodableDynamicCharacter" [testLaws]
  where
    testLaws = testGroup "EncodableDynamicCharacter Laws"
             [ encodeDecodeIdentity
             ]
      where
        encodeDecodeIdentity = testProperty "decodeDynamic alphabet . encodeDynamic alphabet === fmap toList . toList" f
          where
            f :: AlphabetAndCharacter -> Bool
            f alphabetAndDynamicCharacter = lhs dynamicChar == rhs dynamicChar
              where
                enc :: NonEmpty (NonEmpty String) -> DynamicCharacter
                enc = encodeStream alphabet
                lhs = fmap (Set.fromList . toList) . toList . decodeStream alphabet . enc
                rhs = fmap (Set.fromList . toList) . toList
                (alphabet, dynamicChar) = getAlphabetAndCharacter alphabetAndDynamicCharacter


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
      = AlphabetAndCharacter
      { getAlphabetAndCharacter :: (Alphabet String, NonEmpty (NonEmpty String))
      } deriving stock (Eq, Show)


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


-- |
-- Should either pass the test or throw an exception.
equalityWithExceptions :: (Eq a, NFData a, Show a) => a -> a -> Property
equalityWithExceptions x y = monadicIO $ do
    lhs <- supressException x
    rhs <- supressException y
    pure $ case lhs of
             Left  _ -> anyException
             Right a ->
               case rhs of
                 Left  _ -> anyException
                 Right b -> a === b
  where
    supressException :: NFData a => a -> PropertyM IO (Either SomeException a)
    supressException = run . try . evaluate . force

    anyException :: Property
    anyException = True === True
-}


testLaw :: Testable a => a -> String -> String -> TestTree
testLaw f lawName lawExpression = testGroup lawName [testProperty lawExpression f ]


infix 0 -=>
(-=>) :: Testable p => Bool -> p -> Property
(-=>) p q = not p .||. q
