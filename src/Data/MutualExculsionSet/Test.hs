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
    [ semigroupCases
    , semigroupProperties
    ]


testInvariants :: TestTree
testInvariants = testGroup "Invariant corner cases"
    [ minBoundCases
    , infinityCases
    ]


testProperties :: TestTree
testProperties = testGroup "Invariant properties"
    [ orderingProperties
    , additionProperties
    , subtractionProperties
    , multiplicationCases
    , multiplicationProperties
    , divisionProperties
    ]


semigroupCases :: TestTree
semigroupCases = testGroup "semigroup operator specific cases"
    [ testCase "sanity construction" sanityConstruction
    , testCase "simple semigroup operation"   simpleConstruction
    , testCase "symetric annihilation semigroup operation" symetricAnnihilation
    , testCase "left annihilation semigroup operation"     leftAnnihilation
    , testCase "left consistency"                          leftConsistency
    , testCase "right annihilation semigroup operation"    rightAnnihilation
    , testCase "right consistency"                         rightConsistency
    , assertAssociativity (singleton 1 2) (singleton 1 3) $ unsafeFromList [ (1, 0), (2, 3) ]
    , testCase "right bias" $ (singleton 1 3) <> unsafeFromList [ (1, 0), (2, 3) ] @?= mempty
    , testCase "left bias"  $ singleton 1 2 <> singleton 1 3 @?= mempty
    ]
  where
    sanityConstruction =
      singleton True False @?= unsafeFromList [ (True, False) ]
    
    simpleConstruction =
      singleton 1 12 <> singleton 2 21 @?= unsafeFromList [ (1, 12), (2, 21) ]
    
    symetricAnnihilation = 
      singleton 1 2 <> singleton 2 1 @?= mempty
    
    leftAnnihilation =
      singleton 1 2 <> singleton 1 3 @?= mempty

    leftConsistency = findInconsistencies (singleton 1 2 <> singleton 1 3) @?= []
    
    rightAnnihilation =
      singleton 1 3 <> singleton 2 3 @?= mempty
    
    rightConsistency = findInconsistencies (singleton 1 3 <> singleton 2 3) @?= []

    assertAssociativity a b c =
        testCase (unlines [show a, "<>", show b, "<>", show c])
                  $ a <> (b <> c) @?= (a <> b) <> c


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup opperator"
    [ testProperty "op is associative" operationAssocativity
    , testProperty "op is commutive"   operationCommutivity
    ]
  where
    operationAssocativity :: (MutualExculsionSet Int, MutualExculsionSet Int, MutualExculsionSet Int) -> Bool
    operationAssocativity (a, b, c) = a <> (b <> c) == (a <> b) <> c

    operationCommutivity :: (MutualExculsionSet Int, MutualExculsionSet Int) -> Bool
    operationCommutivity (a, b) = a <> b == b <> a



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
    [ testGroup "abelian group under addition"
        [ testProperty "additive identity holds" additiveIdentity
        , localOption (QuickCheckTests 1000000)
            $ testProperty "addition is associative" additiveAssocativity
        , localOption (QuickCheckTests 1000000)
            $ testProperty "addition is commutive"   additiveCommutivity
        ]
    , testGroup "other properties"
        [ testProperty "addition on maxBound is indempotent" additiveUpperBound
        , testProperty "addition of finite values never exceeds maxBound" additiveCeiling
        ]
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


subtractionProperties :: TestTree
subtractionProperties = testGroup "Properties of subtraction"
    [ testProperty "subtraction is the additive inverse" subtractionIsInverse
    , testProperty "subtracting additive identity is indempotent" subtractionIdentity
    , testProperty "subtraction on minBound is indempotent" subtractionLowerBound
    , testProperty "subtraction of finite values never exceeds maxBound" subtractionFloor
    ]
  where
    subtractionIsInverse :: ExtendedNatural -> Bool
    subtractionIsInverse val = val - val == 0 || val == infinity
    
    subtractionIdentity :: ExtendedNatural -> Bool
    subtractionIdentity val = val - 0 == val

    subtractionLowerBound :: ExtendedNatural -> Bool
    subtractionLowerBound val = minBound - val == minBound

    subtractionFloor :: (ExtendedNatural, ExtendedNatural) -> Bool
    subtractionFloor (a, b) = a - b >= minBound


multiplicationCases :: TestTree
multiplicationCases = testGroup "specific multiplication cases"
    [ testGroup "exemplary associativity cases"
        [ assertAssociativity         3326996    4906009    1029418
        , assertAssociativity         2621538    1442243    4131011
        , assertAssociativity        10387400    6746207     415092
        , assertAssociativity         7013604    6385785     529584
        , assertAssociativity          446349   13935261    9720841
        ]
    , testGroup "exemplary distributivity cases"
        [ assertLeftDistributivity 2704487822 3914703344 3465260413
        , assertLeftDistributivity 8252883412 7850920647 5879309961
        ]
    ]
  where
    assertAssociativity :: ExtendedNatural -> ExtendedNatural -> ExtendedNatural -> TestTree
    assertAssociativity a b c =
        testCase (unwords [show a, "*", show b, "*", show c])
          $ a * (b * c) @?= (a * b) * c

    assertLeftDistributivity :: ExtendedNatural -> ExtendedNatural -> ExtendedNatural -> TestTree
    assertLeftDistributivity a b c =
        testCase (unwords [show a, "* (", show b, "+", show c, ")"])
          $ a * (b + c) @?= (a * b) + (a * c)

    


multiplicationProperties :: TestTree
multiplicationProperties = testGroup "Properties of multiplication"
    [ testProperty "multiplicative identity holds" multiplicativeIdentity
    , testProperty "multiplicative annihilation holds" multiplicativeAnnihilation
    , localOption (QuickCheckTests 1000000)
        $ testProperty "multiplication is associative" multiplicativeAssocativity
    , localOption (QuickCheckTests 1000000)
        $ testProperty "multiplication is commutive" multiplicativeCommutivity
    ,  localOption (QuickCheckTests 1000000)
        $ testProperty "multiplication is left-distibutive"  multiplicativeLeftDistributivity
    ,  localOption (QuickCheckTests 1000000)
        $ testProperty "multiplication is right-distibutive" multiplicativeRightDistributivity
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

    multiplicativeLeftDistributivity :: (ExtendedNatural, ExtendedNatural, ExtendedNatural) -> Bool
    multiplicativeLeftDistributivity (a, b, c) = a * (b + c) == (a * b) + (a * c)

    multiplicativeRightDistributivity :: (ExtendedNatural, ExtendedNatural, ExtendedNatural) -> Bool
    multiplicativeRightDistributivity (a, b, c) = (b + c) * a == (b * a) + (c * a)

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
