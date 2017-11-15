{-# LANGUAGE FlexibleInstances #-}

module Numeric.Extended.Natural.Test
  ( testSuite
  ) where

import Numeric.Extended.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

--import Debug.Trace


testSuite :: TestTree
testSuite = testGroup "ExtendedNatural tests"
    [ testInvariants
    , testProperties
    ]


testInvariants :: TestTree
testInvariants = testGroup "Invariant corner cases"
    [ maxBoundCases
    , minBoundCases
    , infinityCases
    ]


testProperties :: TestTree
testProperties = testGroup "Invariant corner cases"
    [ orderingProperties
    , additionProperties
    , multiplicationProperties
    , divisionProperties
    ]


maxBoundCases :: TestTree
maxBoundCases = testGroup "'maxBound' specific cases"
    [ testCase "(maxBound :: ExtendedNatural) == fromFinite (maxBound :: Word) - 1" injectiveDefinitionalValue
    , testCase "unsafeToFinite (maxBound :: ExtendedNatural) == (maxBound :: Word) - 1" surjectiveDefinitionalValue
    , testCase "succ function does not increment maxBound" successorCase
    , testCase "pred function decrements maxBound" predecessorCase
    , testCase "Multiplicative identity holds" multiplicativeIdentityCase
    ]
  where
    injectiveDefinitionalValue =
      (maxBound :: ExtendedNatural) @?= fromFinite ((maxBound :: Word) - 1)

    surjectiveDefinitionalValue =
      unsafeToFinite (maxBound :: ExtendedNatural) @?= (maxBound :: Word) - 1

    successorCase =
      succ (maxBound :: ExtendedNatural) @?= (maxBound :: ExtendedNatural)

    predecessorCase =
      pred (maxBound :: ExtendedNatural) @?= (maxBound :: ExtendedNatural) - 1

    multiplicativeIdentityCase =
      (maxBound :: ExtendedNatural) * 1 @?= (maxBound :: ExtendedNatural)


minBoundCases :: TestTree
minBoundCases = testGroup "'minBound' specific cases"
    [ testCase "(minBound :: ExtendedNatural) == fromFinite (minBound :: Word)" injectiveDefinitionalValue
    , testCase "unsafeToFinite (minBound :: ExtendedNatural) == (minBound :: Word)" surjectiveDefinitionalValue
    , testCase "succ function increment minBound" successorCase
    , testCase "pred function does not decrements minBound" predecessorCase
    ]
  where
    injectiveDefinitionalValue =
      (minBound :: ExtendedNatural) @?= fromFinite (minBound :: Word)

    surjectiveDefinitionalValue =
      unsafeToFinite (minBound :: ExtendedNatural) @?= (minBound :: Word)

    successorCase =
      succ (minBound :: ExtendedNatural) @?= (minBound :: ExtendedNatural) + 1

    predecessorCase =
      pred (minBound :: ExtendedNatural) @?= (minBound :: ExtendedNatural)


infinityCases :: TestTree
infinityCases = testGroup "'infinity' specific cases"
    [ testCase "succ function does not increment 'infinity'" successorCase
    , testCase "pred function does not decrement 'infinity'" predecessorCase
    , testCase "'infinity'     == 'infinity'" identityComparison
    , testCase "'infinity'     >  'maxBound'" maxBoundComparison
    , testCase "'infinity' * 0 == 'infinity'" zeroMultiplication
    ]
  where
    inf = infinity :: ExtendedNatural

    successorCase =
      succ inf @?= inf

    predecessorCase =
      pred inf @?= inf

    identityComparison =
      inf `compare` inf @?= EQ

    maxBoundComparison =
      inf `compare` (maxBound :: ExtendedNatural) @?= GT

    zeroMultiplication =
      inf * 0 @?= inf



orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "The 'compare'  function is reflexively consistent" reflexivity
    , testProperty "The 'infinity' > all finite values" infinityOrdering
    ]
  where
    reflexivity :: (ExtendedNatural, ExtendedNatural) -> Bool
    reflexivity (lhs, rhs) =
      case (lhs `compare` rhs, rhs `compare` lhs) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False

    infinityOrdering :: ExtendedNatural -> Bool
    infinityOrdering val = val == infinity || infinity > val


additionProperties :: TestTree
additionProperties = testGroup "Properties of addition"
    [ testProperty "additive identity holds" additiveIdentity
    , testProperty "addition is associative" additiveAssocativity
    , testProperty "addition is commutive" additiveCommutivity
    , testProperty "addition on maxBound is indempotent" additiveUpperBound
    , testProperty "addition of finite values never exceeds maxBound" additiveCeiling
    ]
  where
    additiveIdentity :: ExtendedNatural -> Bool
    additiveIdentity val = 0 + val == val

    additiveAssocativity :: (ExtendedNatural, ExtendedNatural, ExtendedNatural) -> Bool
    additiveAssocativity (a, b, c) = a + (b + c) == (a + b) + c

    additiveCommutivity :: (ExtendedNatural, ExtendedNatural) -> Bool
    additiveCommutivity (a, b) = a + b == b + a

    additiveUpperBound :: ExtendedNatural -> Bool
    additiveUpperBound val = maxBound + val == maxBound || val == infinity

    additiveCeiling :: (ExtendedNatural, ExtendedNatural) -> Bool
    additiveCeiling (a, b) = a + b <= maxBound || a == infinity || b == infinity


multiplicationProperties :: TestTree
multiplicationProperties = testGroup "Properties of multiplication"
    [ testProperty "multiplicative identity holds" multiplicativeIdentity
    , testProperty "multiplicative annihilation holds" multiplicativeAnnihilation
    , testProperty "multiplication is associative" multiplicativeAssocativity
    , testProperty "multiplication is commutive" multiplicativeCommutivity
    , testProperty "multiplication on maxBound is indempotent (except 0 & infinity)" multiplicativeUpperBound
    , testProperty "multiplication of finite values never exceeds maxBound" multiplicativeCeiling
    ]
  where
    multiplicativeIdentity :: ExtendedNatural -> Bool
    multiplicativeIdentity val = 1 * val == val

    multiplicativeAnnihilation :: ExtendedNatural -> Bool
    multiplicativeAnnihilation val = 0 * val == 0 || val == infinity

    multiplicativeAssocativity :: (ExtendedNatural, ExtendedNatural, ExtendedNatural) -> Bool
    multiplicativeAssocativity (a, b, c) = a * (b * c) == (a * b) * c

    multiplicativeCommutivity :: (ExtendedNatural, ExtendedNatural) -> Bool
    multiplicativeCommutivity (a, b) = a * b == b * a

    multiplicativeUpperBound :: ExtendedNatural -> Bool
    multiplicativeUpperBound val = maxBound * val == maxBound || val == infinity || val == 0

    multiplicativeCeiling :: (ExtendedNatural, ExtendedNatural) -> Bool
    multiplicativeCeiling (a, b) = a * b <= maxBound || a == infinity || b == infinity


divisionProperties :: TestTree
divisionProperties = testGroup "Properties of division"
    [ testProperty "division identity holds"                    divisionIdentity
    , testProperty "division of infinite numerator is infinity" divisionInfiniteNumerator
    , testProperty "division by infinite denominator zero"      divisionInfiniteDenominator
    , testProperty "division by zero denominator is infinity"   divisionZeroDenominator
    ]
  where
    divisionIdentity :: ExtendedNatural -> Bool
    divisionIdentity val = val `div` val == 1 || val == 0 || val == infinity

    divisionInfiniteNumerator :: ExtendedNatural -> Bool
    divisionInfiniteNumerator val = infinity `div` val == infinity

    divisionInfiniteDenominator :: ExtendedNatural -> Bool
    divisionInfiniteDenominator val = val `div` infinity == 0 || val == infinity

    divisionZeroDenominator :: ExtendedNatural -> Bool
    divisionZeroDenominator val = val `div` 0 == infinity
