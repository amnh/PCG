{-# LANGUAGE FlexibleInstances #-}

module Numeric.NonNegativeAverage.Test
  ( testSuite
  ) where


import Data.Ratio
import Data.Semigroup
import Numeric.NonNegativeAverage
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "NonNegativeAverage tests"
    [ testProperties
    ]


testProperties :: TestTree
testProperties = testGroup "Invariant properties"
    [ orderingProperties
    , semigroupProperties
    ]


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "ordering preserves symetry"  symetry
    , testProperty "order preserving projection" orderPreserving
    ]
  where
    orderPreserving :: (Word, Word) -> Bool
    orderPreserving (a, b) = a `compare` b == fromNonNegativeValue a `compare` fromNonNegativeValue b
    
    symetry :: (NonNegativeAverage, NonNegativeAverage) -> Bool
    symetry (a, b) =
      case (a `compare` b, b `compare` a) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup opperator"
    [ testProperty "op identity holds" projectionIdentity
    , testProperty "op is associative" operationAssocativity
    , testProperty "op is commutive"   operationCommutivity
    ]
  where
    projectionIdentity :: Word -> Bool
    projectionIdentity x = let (n,d) = f x in n == x && d == 1
      where
        f :: Word -> (Word, Word)
        f = ((,) <$> fromInteger . numerator <*> fromInteger . denominator) . g
        
        g :: Word -> Rational
        g = fromNonNegativeAverage . fromNonNegativeValue

    operationAssocativity :: (NonNegativeAverage, NonNegativeAverage, NonNegativeAverage) -> Bool
    operationAssocativity (a, b, c) = a <> (b <> c) == (a <> b) <> c

    operationCommutivity :: (NonNegativeAverage, NonNegativeAverage) -> Bool
    operationCommutivity (a, b) = a <> b == b <> a
