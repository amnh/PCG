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
    ]


-- Just to make sure that rows . fromRows == id.
testInvariants :: TestTree
testInvariants = testGroup "Invariant corner cases"
    [ maxBoundCases
    , minBoundCases
    , infinityCases
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
    , testCase "'infinity' == 'infinity'" identityComparison
    , testCase "'infinity' >  'maxBound'" maxBoundComparison
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

