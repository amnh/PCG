{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module System.ErrorPhase.Test
  ( testSuite
  ) where

import Control.DeepSeq
import System.ErrorPhase
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding ((=/=))


testSuite :: TestTree
testSuite = testGroup "ErrorPhase type"
    [ equalityLaws       @ErrorPhase
    , normalFormDataLaws @ErrorPhase
    , orderingLaws       @ErrorPhase
    , showProperties     @ErrorPhase
    ]


equalityLaws
  :: forall a.
     ( Arbitrary a
     , Eq a
     , Show a
     )
  => TestTree
equalityLaws = testGroup "Equality Laws"
    [ testLaw negation     "Negation"     "x /= y ==> not (x == y)"
    , testLaw symmetry      "Symmetry"      "x /= y ==> y /= x"
    , testLaw transitivity "Transitivity" "x == y && y == z ==> x == z"
    , testLaw refexivity   "Reflexivity"  "x == x"
    ]
  where
    negation :: a -> a -> Property
    negation x y =
        x /= y ==> not (x == y)

    symmetry :: a -> a -> Property
    symmetry x y =
        x /= y ==> y =/= x

    transitivity :: a -> a -> a -> Property
    transitivity x y z =
        not (x == y && y == z) .||. x == z

    refexivity :: a -> Property
    refexivity x =
        x === x


normalFormDataLaws
  :: forall a.
     ( Arbitrary a
     , NFData a
     , Show a
     )
  => TestTree
normalFormDataLaws = testGroup "NFData Laws"
    [ testLaw finiteReduction "Finiteness" "rnf x =/= _|_"
    ]
  where
    finiteReduction :: a -> Property
    finiteReduction x =
        rnf x === ()


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
        (x < y && y < z) -=> x < z

    transitivity2 :: a -> a -> a -> Property
    transitivity2 x y z =
        (x > y && y > z) -=> x > z


showProperties
  :: forall a.
     ( Arbitrary a
     , Show a
     )
  => TestTree
showProperties = testGroup "Show Laws"
    [ testLaw finiteString  "Finiteness" "rnf (show x) =/= _|_"
    , testLaw nonNullString "Non-null"   "not . null . show"
    ]
  where
    finiteString :: a -> Property
    finiteString x =
        (rnf . show) x === ()

    nonNullString :: a -> Property
    nonNullString =
        isNonEmpty . show


testLaw :: Testable a => a -> String -> String -> TestTree
testLaw f lawName lawExpression = testGroup lawName [testProperty lawExpression f ]


isNonEmpty :: (Eq a, Monoid a, Show a) => a -> Property
isNonEmpty x = counterexample (show x <> " == mempty") (x /= mempty)


-- |
-- Like '/=', but prints a counterexample when it fails.
infix 4 =/=
(=/=) :: (Eq a, Show a) => a -> a -> Property
(=/=) x y = counterexample (show x <> " == " <> show y) (x /= y)


-- |
-- Like '==>' but for case where the left-hand side is hard to generate.
infix 3 -=>
(-=>) :: Bool -> Bool -> Property
(-=>) x y = not x .||. y

