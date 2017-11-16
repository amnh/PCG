{-# LANGUAGE FlexibleInstances #-}

module Numeric.Extended.Real.Test
  ( testSuite
  ) where

import Numeric.Extended.Real
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "ExtendedReal tests"
    [ testInvariants
    , testProperties
    ]


testInvariants :: TestTree
testInvariants = testGroup "Invariant corner cases"
    [ infinityCases
    ]


testProperties :: TestTree
testProperties = testGroup "Invariant properties"
    [ orderingProperties
    , additionProperties
    , subtractionProperties
    , multiplicationProperties
    , divisionProperties
    ]


infinityCases :: TestTree
infinityCases = testGroup "'infinity' specific cases"
    [ testCase "succ function does not increment 'infinity'" successorCase
    , testCase "pred function does not decrement 'infinity'" predecessorCase
    , testCase "'infinity'     == 'infinity'" identityComparison
    , testCase "'infinity' * 0 == 'infinity'" zeroMultiplication
    ]
  where
    inf = infinity :: ExtendedReal

    successorCase =
      succ inf @?= inf

    predecessorCase =
      pred inf @?= inf

    identityComparison =
      inf `compare` inf @?= EQ

    zeroMultiplication =
      inf * 0 @?= inf



orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "The 'compare'  function is reflexively consistent" reflexivity
    , testProperty "The 'infinity' > all finite values" infinityOrdering
    ]
  where
    reflexivity :: (ExtendedReal, ExtendedReal) -> Bool
    reflexivity (lhs, rhs) =
      case (lhs `compare` rhs, rhs `compare` lhs) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False

    infinityOrdering :: ExtendedReal -> Bool
    infinityOrdering val = val == infinity || infinity > val


additionProperties :: TestTree
additionProperties = testGroup "Properties of addition"
    [ testProperty "additive identity holds" additiveIdentity
--    , testProperty "addition is associative" additiveAssocativity
    , testProperty "addition is commutive" additiveCommutivity
    , testProperty "addition on maxBound is indempotent" additiveUpperBound
    , testProperty "addition of finite values never exceeds maxBound" additiveCeiling
    ]
  where
    additiveIdentity :: ExtendedReal -> Bool
    additiveIdentity val = 0 + val == val

-- Can't test associativity because of rounding errors
--    additiveAssocativity :: (ExtendedReal, ExtendedReal, ExtendedReal) -> Bool
--    additiveAssocativity (a, b, c) = a + (b + c) ~== (a + b) + c

    additiveCommutivity :: (ExtendedReal, ExtendedReal) -> Bool
    additiveCommutivity (a, b) = a + b == b + a

    additiveUpperBound :: ExtendedReal -> Bool
    additiveUpperBound val = maxBound + val == maxBound || val == infinity

    additiveCeiling :: (ExtendedReal, ExtendedReal) -> Bool
    additiveCeiling (a, b) = a + b <= maxBound || a == infinity || b == infinity


subtractionProperties :: TestTree
subtractionProperties = testGroup "Properties of subtraction"
    [ testProperty "subtracting additive identity is indempotent" subtractionIdentity
    , testProperty "subtraction on minBound is indempotent" subtractionLowerBound
    , testProperty "subtraction of finite values never exceeds maxBound" subtractionFloor
    ]
  where
    subtractionIdentity :: ExtendedReal -> Bool
    subtractionIdentity val = val - 0 == val

    subtractionLowerBound :: ExtendedReal -> Bool
    subtractionLowerBound val = minBound - val == minBound

    subtractionFloor :: (ExtendedReal, ExtendedReal) -> Bool
    subtractionFloor (a, b) = a - b >= minBound


multiplicationProperties :: TestTree
multiplicationProperties = testGroup "Properties of multiplication"
    [ testProperty "multiplicative identity holds" multiplicativeIdentity
    , testProperty "multiplicative annihilation holds" multiplicativeAnnihilation
--    , testProperty "multiplication is associative" multiplicativeAssocativity
    , testProperty "multiplication is commutive" multiplicativeCommutivity
    , testProperty "multiplication of finite values with maxBound is non-increasing" multiplicativeUpperBound
    , testProperty "multiplication of finite values never exceeds maxBound" multiplicativeCeiling
    ]
  where
    multiplicativeIdentity :: ExtendedReal -> Bool
    multiplicativeIdentity val = 1 * val == val

    multiplicativeAnnihilation :: ExtendedReal -> Bool
    multiplicativeAnnihilation val = 0 * val == 0 || val == infinity

-- Can't test associativity because of rounding errors
--    multiplicativeAssocativity :: (ExtendedReal, ExtendedReal, ExtendedReal) -> Bool
--    multiplicativeAssocativity (a, b, c) = a * (b * c) ~== (a * b) * c

    multiplicativeCommutivity :: (ExtendedReal, ExtendedReal) -> Bool
    multiplicativeCommutivity (a, b) = a * b == b * a

    multiplicativeUpperBound :: ExtendedReal -> Bool
    multiplicativeUpperBound val = maxBound * val <= maxBound || val == infinity

    multiplicativeCeiling :: (ExtendedReal, ExtendedReal) -> Bool
    multiplicativeCeiling (a, b) = a * b <= maxBound || a == infinity || b == infinity


divisionProperties :: TestTree
divisionProperties = testGroup "Properties of division"
    [ testProperty "division identity holds"                    divisionIdentity
    , testProperty "division of infinite numerator is infinity" divisionInfiniteNumerator
    , testProperty "division by infinite denominator zero"      divisionInfiniteDenominator
    , testProperty "division by zero denominator is infinity"   divisionZeroDenominator
    ]
  where
    divisionIdentity :: ExtendedReal -> Bool
    divisionIdentity val = val / val == 1 || val == 0 || val == infinity

    divisionInfiniteNumerator :: ExtendedReal -> Bool
    divisionInfiniteNumerator val = infinity / val == infinity

    divisionInfiniteDenominator :: ExtendedReal -> Bool
    divisionInfiniteDenominator val = val / infinity == 0 || val == infinity

    divisionZeroDenominator :: ExtendedReal -> Bool
    divisionZeroDenominator val = val / 0 == infinity
