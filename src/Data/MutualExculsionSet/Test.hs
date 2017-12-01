{-# LANGUAGE FlexibleInstances #-}

module Data.MutualExculsionSet.Test
  ( testSuite
  ) where


import Data.Semigroup
import Data.MutualExculsionSet.Internal
import Numeric.Extended.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "MutualExcludionSet semigroup tests"
    [ orderingProperties
    , semigroupCases
    , semigroupProperties
    ]


semigroupCases :: TestTree
semigroupCases = testGroup "semigroup operator specific cases"
    [ testGroup "constructions"
      [ testCase "sanity construction"        sanityConstruction
      , testCase "indempotent construction"   indempotentConstruction
      , testCase "simple semigroup operation" simpleConstruction
      ]
    , testGroup "paradoxes"
      [ testCase "paradox singleton"          paradoxSingleton
      , testCase "paradox construction"       paradoxConstruction
      , testCase "paradox in join"            paradoxPermissible
      ]
    ]
  where
    sanityConstruction =
      singleton True False @?= unsafeFromList [ (True, False) ]
    
    simpleConstruction =
      singleton 1 2 <> singleton 3 4 @?= unsafeFromList [ (1, 2), (3, 4) ]

    indempotentConstruction =
      singleton 1 2 <> singleton 1 2 @?= singleton 1 2

    paradoxSingleton =
      singleton 1 1 @?= unsafeFromList [ (1, 1) ]
    
    paradoxConstruction = 
      singleton 1 2  <> singleton 2 1 @?= unsafeFromList [ (1, 2), (2, 1) ]
    
    paradoxPermissible = lhs <> rhs @?= unsafeFromList [ (1, 2), (2, 1), (3, 4), (5, 6) ]
      where
        lhs = (singleton 1 2 <> singleton 3 4)
        rhs = (singleton 2 1 <> singleton 5 6)

    inclusionSetViolation =
      (singleton 1 2 <> singleton 1 3) @?= unsafeFromList [ (1, 3) ]

    exclusionSetViolation =
      (singleton 1 3 <> singleton 2 3) @?= unsafeFromList [ (2, 3) ]


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup operator"
    [ localOption (QuickCheckTests 10000)
        $ testProperty "(<>) is associative" operationAssocativity
    , localOption (QuickCheckTests  1000)
        $ testProperty "(<>) is commutative" operationCommutativity
    ]
  where
    operationAssocativity :: (MutualExculsionSet Int, MutualExculsionSet Int, MutualExculsionSet Int) -> Bool
    operationAssocativity (a, b, c) = a <> (b <> c) == (a <> b) <> c

    operationCommutativity :: (MutualExculsionSet Int, MutualExculsionSet Int) -> Bool
    operationCommutativity (a, b) = a <> b == b <> a


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "order preserving projection" orderPreserving
    , testProperty "ordering preserves symetry"  symetry
    ]
  where
    orderPreserving :: ((Word, Word), (Word, Word)) -> Bool
    orderPreserving (lhs@(a, b), rhs@(c, d)) =
      lhs `compare` rhs == singleton a b `compare` singleton c d

    symetry :: (ExtendedNatural, ExtendedNatural) -> Bool
    symetry (lhs, rhs) =
      case (lhs `compare` rhs, rhs `compare` lhs) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False
