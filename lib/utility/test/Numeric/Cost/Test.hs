{-# LANGUAGE FlexibleInstances #-}

module Numeric.Cost.Test
  ( testSuite
  ) where

import Control.DeepSeq
import Numeric.Cost
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "Cost tests"
    [ testInvariantCases
    , testInvariantProperties
    ]


testInvariantCases :: TestTree
testInvariantCases = testGroup "Invariant corner cases"
    [ infinityCases
    ]


testInvariantProperties :: TestTree
testInvariantProperties = testGroup "Invariant properties"
    [ equalityProperties
    , orderingProperties
    , normalFormDataProperties
    , showProperties
    , semigroupProperties
    , monoidProperties
    ]


infinityCases :: TestTree
infinityCases = testGroup "'infinity' specific cases"
    [ testCase "'infinity'     == 'infinity'" identityComparison
    ]
  where
    inf = infinity :: Cost

    identityComparison =
      inf `compare` inf @?= EQ


equalityProperties :: TestTree
equalityProperties = testGroup "Equality Laws"
    [ testLaw negation     "Negation"     "x /= y ==> not (x == y)"
    , testLaw symetry      "Symetry"      "x /= y ==> y /= x"
    , testLaw transitivity "Transitivity" "x == y && y == z ==> x == z"
    , testLaw refexivity   "Reflexivity"  "x == x"
    ]
  where
    negation :: Cost -> Cost -> Property
    negation x y =
        x /= y ==> not (x == y)

    symetry :: Cost -> Cost -> Property
    symetry x y =
        x /= y ==> y =/= x

    transitivity :: Cost -> Cost -> Cost -> Property
    transitivity x y z =
        not (x == y && y == z) .||. x == z

    refexivity :: Cost -> Property
    refexivity x =
        x === x


normalFormDataProperties :: TestTree
normalFormDataProperties = testGroup "NFData Laws"
    [ testLaw finiteReduction "Finiteness" "rnf x =/= _|_"
    ]
  where
    finiteReduction :: Cost -> Property
    finiteReduction x =
        rnf x === ()


orderingProperties :: TestTree
orderingProperties = testGroup "Ordering Laws"
    [ testLaw symetry       "Symetry"       "x >= y ==> y <= x"
    , testLaw transitivity1 "Transitive I"  "x < y && y < z ==> x < z"
    , testLaw transitivity2 "Transitive II" "x > y && y > z ==> x > z"
    , testProperty "The 'infinity' > all finite values" infinityOrdering
    ]
  where
    symetry :: Cost -> Cost -> Bool
    symetry lhs rhs =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity1 :: Cost -> Cost -> Cost -> Property
    transitivity1 x y z =
        (x < y && y < z) ==> x < z

    transitivity2 :: Cost -> Cost -> Cost -> Property
    transitivity2 x y z =
        (x > y && y > z) ==> x > z

    infinityOrdering :: Cost -> Bool
    infinityOrdering val = val == infinity || infinity > val


monoidProperties :: TestTree
monoidProperties = testGroup "Properties of this semigroup operator"
    [ testProperty "left identity" leftIdentity
    , testProperty "right identity" rightIdentity
    ]
  where
    leftIdentity :: Cost -> Property
    leftIdentity a = mempty <> a === a

    rightIdentity :: Cost -> Property
    rightIdentity a = a <> mempty === a


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup operator"
    [ localOption (QuickCheckTests 10000)
        $ testProperty "(<>) is associative" operationAssocativity
    , localOption (QuickCheckTests  1000)
        $ testProperty "(<>) is commutative" operationCommutativity
    ]
  where
    operationAssocativity :: (Cost, Cost, Cost) -> Property
    operationAssocativity (a, b, c) = a <> (b <> c) === (a <> b) <> c

    operationCommutativity :: (Cost, Cost) -> Property
    operationCommutativity (a, b) = a <> b === b <> a


showProperties :: TestTree
showProperties = testGroup "Properties of string rendering"
    [ testLaw finiteString  "Finiteness" "rnf (show x) =/= _|_"
    , testLaw nonNullString "Non-null"   "not . null . show"
    ]
  where
    finiteString :: Cost -> Property
    finiteString x =
        (rnf . show) x === ()

    nonNullString :: Cost -> Bool
    nonNullString =
        not . null . show


testLaw :: Testable a => a -> String -> String -> TestTree
testLaw f lawName lawExpression = testGroup lawName [testProperty lawExpression f ]
