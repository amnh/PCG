{-# LANGUAGE FlexibleInstances #-}

module Data.BitVector.Test
  ( testSuite
  ) where

import Data.Bits
import Data.BitVector.LittleEndian
import Data.Functor.Compose
import Data.Functor.Identity
--import Data.Foldable
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ bitsTests
    , finiteBitsTests
    , monoFunctorProperties
    , monoFoldableProperties
    , monoidProperties
    , monoTraversableProperties
    , orderingProperties
    , semigroupProperties
    , bitVectorProperties
    ]


bitsTests :: TestTree
bitsTests = testGroup "Bits instance properties"
    [ testProperty "∀ n ≥ 0, clearBit zeroBits n === zeroBits" zeroBitsAndClearBit
    , testProperty "∀ n ≥ 0, setBit   zeroBits n === bit n" zeroBitsAndSetBit
    , testProperty "∀ n ≥ 0, testBit  zeroBits n === False" zeroBitsAndTestBit
    , testCase     "         popCount zeroBits   === 0" zeroBitsAndPopCount
    , testProperty "complement === omap not" complementOmapNot
    , testProperty "(`setBit` n) === (.|. bit n)" setBitDefinition
    , testProperty "(`clearBit` n) === (.&. complement (bit n))" clearBitDefinition
    , testProperty "(`complementBit` n) === (`xor` bit n)" complementBitDefinition
    , testProperty "(`testBit` n) . (`setBit` n)" testBitAndSetBit
    , testProperty "not  . (`testBit` n) . (`clearBit` n)" testBitAndClearBit
    ]
  where
    zeroBitsAndClearBit :: NonNegative Int -> Property
    zeroBitsAndClearBit (NonNegative n) =
        clearBit (zeroBits :: BitVector) n === zeroBits

    zeroBitsAndSetBit :: NonNegative Int -> Property
    zeroBitsAndSetBit (NonNegative n) =
        setBit   (zeroBits :: BitVector) n === bit n

    zeroBitsAndTestBit :: NonNegative Int -> Property
    zeroBitsAndTestBit (NonNegative n) =
        testBit  (zeroBits :: BitVector) n === False

    zeroBitsAndPopCount :: Assertion
    zeroBitsAndPopCount =
        popCount (zeroBits :: BitVector) @?= 0

    complementOmapNot :: BitVector -> Property
    complementOmapNot bv =
        complement bv === omap not bv

    setBitDefinition :: (NonNegative Int, BitVector) -> Property
    setBitDefinition (NonNegative n, bv) =
        bv `setBit` n === bv .|. bit n

    clearBitDefinition :: (NonNegative Int, BitVector) -> Property
    clearBitDefinition (NonNegative n, bv) =
        n < (fromEnum . dimension) bv ==>
          (bv `clearBit` n === bv .&. complement  (zed .|. bit n))
      where
        zed = bitvector (dimension bv) (0 :: Integer)

    complementBitDefinition :: (NonNegative Int, BitVector) -> Property
    complementBitDefinition (NonNegative n, bv) =
        bv `complementBit` n === bv `xor` bit n

    testBitAndSetBit :: (NonNegative Int, BitVector) -> Bool
    testBitAndSetBit (NonNegative n, bv) =
        ((`testBit` n) . (`setBit` n)) bv

    testBitAndClearBit :: (NonNegative Int, BitVector) -> Bool
    testBitAndClearBit (NonNegative n, bv) =
        (not  . (`testBit` n) . (`clearBit` n)) bv


finiteBitsTests :: TestTree
finiteBitsTests = testGroup "FiniteBits instance consistency"
    [ testProperty "fromEnum . dimension === finiteBitSize" finiteBitSizeIsDimension
    , testProperty "length . toBits === finiteBitSize" finiteBitSizeIsBitLength
    , testProperty "length . takeWhile not === countLeadingZeros . fromBits" countLeadingZeroAndFromBits
    , testProperty "length . takeWhile not . toBits === countLeadingZeros" countLeadingZeroAndToBits
    , testProperty "length . takeWhile not . reverse === countTrailingZeros . fromBits" countTrailingZeroAndFromBits
    , testProperty "length . takeWhile not . reverse . toBits === countTrailingZeros" countTrailingZeroAndToBits
    ]
  where
    finiteBitSizeIsDimension :: BitVector -> Property
    finiteBitSizeIsDimension bv =
      (fromEnum . dimension) bv === finiteBitSize bv

    finiteBitSizeIsBitLength :: BitVector -> Property
    finiteBitSizeIsBitLength bv =
      (length . toBits) bv === finiteBitSize bv

    countLeadingZeroAndFromBits :: [Bool] -> Property
    countLeadingZeroAndFromBits bs =
      (length . takeWhile not) bs === (countLeadingZeros . fromBits) bs

    countLeadingZeroAndToBits :: BitVector -> Property
    countLeadingZeroAndToBits bv =
      (length . takeWhile not . toBits) bv === countLeadingZeros bv

    countTrailingZeroAndFromBits :: [Bool] -> Property
    countTrailingZeroAndFromBits bs =
      (length . takeWhile not . reverse) bs === (countTrailingZeros . fromBits) bs

    countTrailingZeroAndToBits :: BitVector -> Property
    countTrailingZeroAndToBits bv =
      (length . takeWhile not . reverse . toBits) bv === countTrailingZeros bv


monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properites of a MonoFunctor"
    [ testProperty "omap id === id" omapId
    , testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: BitVector -> Property
    omapId bv = omap id bv === id bv

    omapComposition :: (Blind (Bool -> Bool), Blind (Bool -> Bool), BitVector) -> Property
    omapComposition (Blind f, Blind g, bv) = omap (f . g) bv ===  (omap f . omap g) bv


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
    testFoldrFoldMap :: (Blind (Bool -> Word -> Word), Word, BitVector) -> Property
    testFoldrFoldMap (Blind f, z, bv) =
        ofoldr f z bv === appEndo (ofoldMap (Endo . f) bv) z

    testFoldlFoldMap :: (Blind (Word -> Bool -> Word), Word, BitVector) -> Property
    testFoldlFoldMap (Blind f, z, bv) =
        ofoldl' f z bv === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bv)) z

    testFoldr :: (Blind (Bool -> Word -> Word), Word, BitVector) -> Property
    testFoldr (Blind f, z, bv) =
        ofoldr f z bv === (ofoldr f z . otoList) bv

    testFoldl :: (Blind (Word -> Bool -> Word), Word, BitVector) -> Property
    testFoldl (Blind f, z, bv) =
        ofoldl' f z bv === (ofoldl' f z . otoList) bv

    testFoldr1 :: (Blind (Bool -> Bool -> Bool), BitVector) -> Property
    testFoldr1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldr1Ex f bv === (ofoldr1Ex f . otoList) bv

    testFoldl1 :: (Blind (Bool -> Bool -> Bool), BitVector) -> Property
    testFoldl1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldl1Ex' f bv === (ofoldl1Ex' f . otoList) bv

    testAll :: (Blind (Bool -> Bool), BitVector) -> Property
    testAll (Blind f, bv) =
        oall f bv === (getAll . ofoldMap (All . f)) bv

    testAny :: (Blind (Bool -> Bool), BitVector) -> Property
    testAny (Blind f, bv) =
        oany f bv === (getAny . ofoldMap (Any . f)) bv

    testLength :: BitVector -> Property
    testLength bv =
        olength bv === (length . otoList) bv

    testNull :: BitVector -> Property
    testNull bv =
        onull bv === ((0 ==) . olength) bv

    testHead :: BitVector -> Property
    testHead bv =
        (not . onull) bv ==> headEx bv === (getFirst . ofoldMap1Ex First) bv

    testTail :: BitVector -> Property
    testTail bv =
        (not . onull) bv ==> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Bool, BitVector) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


monoidProperties :: TestTree
monoidProperties = testGroup "Properties of a Monoid"
    [ testProperty "left identity" leftIdentity
    , testProperty "right identity" rightIdentity
    ]
  where
    leftIdentity :: BitVector -> Property
    leftIdentity a = mempty <> a === a

    rightIdentity :: BitVector -> Property
    rightIdentity a = a <> mempty === a


monoTraversableProperties :: TestTree
monoTraversableProperties = testGroup "Properties of MonoTraversable"
    [ testProperty "t . otraverse f === otraverse (t . f)" testNaturality
    , testProperty "otraverse Identity === Identity" testIdentity
    , testProperty "otraverse (Compose . fmap g . f) === Compose . fmap (otraverse g) . otraverse f" testComposition
    ]
  where
    testNaturality :: (Blind (Bool -> [Bool]), BitVector) -> Property
    testNaturality (Blind f, bv) =
        (headMay . otraverse f) bv === otraverse (headMay . f) bv

    testIdentity :: BitVector -> Property
    testIdentity bv =
        otraverse Identity bv === Identity bv

    testComposition :: (Blind (Bool -> Either Word Bool), Blind (Bool -> Maybe Bool), BitVector) -> Property
    testComposition (Blind f, Blind g, bv) =
        otraverse (Compose . fmap g . f) bv === (Compose . fmap (otraverse g) . otraverse f) bv


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of an Ordering"
    [ testProperty "ordering preserves symetry"  symetry
    , testProperty "ordering is transitive (total)" transitivity
    ]
  where
    symetry :: (BitVector, BitVector) -> Bool
    symetry (lhs, rhs) =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity :: (BitVector, BitVector, BitVector) -> Property
    transitivity (a, b, c) = caseOne .||. caseTwo
      where
        caseOne = (a <= b && b <= c) ==> a <= c
        caseTwo = (a >= b && b >= c) ==> a >= c


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of a Semigroup"
    [ localOption (QuickCheckTests 10000)
        $ testProperty "(<>) is associative" operationAssocativity
    ]
  where
    operationAssocativity :: (BitVector, BitVector, BitVector) -> Property
    operationAssocativity (a, b, c) = a <> (b <> c) === (a <> b) <> c


bitVectorProperties :: TestTree
bitVectorProperties = testGroup "BitVector properties"
    [ testProperty "otoList === toBits" otoListTest
    , testProperty "dimension === length . toBits" dimensionAndToBits
    , testProperty "dimension === finiteBitSize" dimensionAndFiniteBitSize
    , testProperty "fromBits . toBits === id" toBitsFromBits
    , testCase     "isZeroVector zeroBits" zeroBitsIsZeroVector
    , testProperty "isZeroVector === (0 ==) . popCount" popCountAndZeroVector
    , testProperty "isZeroVector === all not . toBits" zeroVectorAndAllBitsOff
    , testProperty "(0 ==) . toUnsignedNumber ==> isZeroVector" toUnsignedNumImpliesZeroVector
    , testProperty "toUnsignedNumber . bitvector === id" bitVectorUnsignedNumIdentity
    ]
  where
    otoListTest :: BitVector -> Property
    otoListTest bv =
        otoList bv === toBits bv

    dimensionAndToBits :: BitVector -> Property
    dimensionAndToBits bv =
        (fromEnum . dimension) bv === (length . toBits) bv

    dimensionAndFiniteBitSize :: BitVector -> Property
    dimensionAndFiniteBitSize bv =
        (fromEnum . dimension) bv === finiteBitSize bv

    toBitsFromBits :: BitVector -> Property
    toBitsFromBits bv =
        (fromBits . toBits) bv === bv

    popCountAndZeroVector :: BitVector -> Property
    popCountAndZeroVector bv =
        isZeroVector bv === ((0 ==) . popCount) bv

    zeroVectorAndAllBitsOff :: BitVector -> Property
    zeroVectorAndAllBitsOff bv =
        isZeroVector bv === (all not . toBits) bv

    toUnsignedNumImpliesZeroVector :: BitVector -> Property
    toUnsignedNumImpliesZeroVector bv =
        ((0 ==) . (toUnsignedNumber :: BitVector -> Integer)) bv ==> isZeroVector bv

    zeroBitsIsZeroVector :: Assertion
    zeroBitsIsZeroVector = assertBool "zeroBits is not a 'zero vector'" $ isZeroVector zeroBits

    bitVectorUnsignedNumIdentity :: NonNegative Integer -> Property
    bitVectorUnsignedNumIdentity (NonNegative num) =
        (toUnsignedNumber . bitvector width) num === num
      where
        width = succ . ceiling . logBase (2.0 :: Double) $ fromIntegral num
