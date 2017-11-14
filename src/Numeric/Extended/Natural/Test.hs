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
    ]


maxBoundCases :: TestTree
maxBoundCases = testGroup "'maxBound' specific cases"
    [ testCase "(maxBound :: ExtendedNatural) ==  (maxBound :: Word) - 1" definitionalValue
    , testCase "succ function does not increment maxBound" successorCase
    , testCase "pred function decraments maxBound" predecessorCase
    , testCase "Multiplicative identity holds" multiplicativeIdentityCase
    ]
  where
    definitionalValue =
      (maxBound :: ExtendedNatural) @?= fromFinite ((maxBound :: Word) - 1)

    successorCase =
      succ (maxBound :: ExtendedNatural) @?= (maxBound :: ExtendedNatural)

    predecessorCase =
      pred (maxBound :: ExtendedNatural) @?= (maxBound :: ExtendedNatural) - 1

    multiplicativeIdentityCase =
      (maxBound :: ExtendedNatural) * 1 @?= (maxBound :: ExtendedNatural)
