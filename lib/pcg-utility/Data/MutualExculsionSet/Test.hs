{-# LANGUAGE FlexibleInstances #-}

module Data.MutualExculsionSet.Test
  ( testSuite
  ) where


import Data.Foldable
import Data.Semigroup
import Data.MutualExculsionSet.Internal
import Numeric.Extended.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "MutualExcludionSet semigroup tests"
    [ constructionCases
    , orderingProperties
    , semigroupProperties
    , monoidProperties
    , structuralProperties
    ]


constructionCases :: TestTree
constructionCases = testGroup "construction specific cases"
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
      singleton 1 2 <> singleton 3 4 @?= unsafeFromList [ (1, 2), (3, 4 :: Word) ]

    indempotentConstruction =
      singleton 1 2 <> singleton 1 2 @?= singleton 1 (2 :: Word)

    paradoxSingleton =
      singleton 1 1 @?= unsafeFromList [ (1, 1 :: Word) ]
    
    paradoxConstruction = 
      singleton 1 2  <> singleton 2 1 @?= unsafeFromList [ (1, 2), (2, 1 :: Word) ]
    
    paradoxPermissible = lhs <> rhs @?= unsafeFromList [ (1, 2), (2, 1), (3, 4), (5, 6 :: Word) ]
      where
        lhs = singleton 1 2 <> singleton 3 4 :: MutualExculsionSet Word
        rhs = singleton 2 1 <> singleton 5 6 :: MutualExculsionSet Word

{-
    inclusionSetViolation =
      (singleton 1 2 <> singleton 1 3) @?= unsafeFromList [ (1, 3) ]

    exclusionSetViolation =
      (singleton 1 3 <> singleton 2 3) @?= unsafeFromList [ (2, 3) ]
-}


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "order preserving projection" orderPreserving
    , testProperty "ordering preserves symetry"  symetry
    ]
  where
    orderPreserving :: ((Word, Word), (Word, Word)) -> Property
    orderPreserving (lhs@(a, b), rhs@(c, d)) =
      lhs `compare` rhs === singleton a b `compare` singleton c d

    symetry :: (ExtendedNatural, ExtendedNatural) -> Bool
    symetry (lhs, rhs) =
      case (lhs `compare` rhs, rhs `compare` lhs) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False


semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup operator"
    [ localOption (QuickCheckTests 10000)
        $ testProperty "(<>) is associative" operationAssocativity
    , localOption (QuickCheckTests  1000)
        $ testProperty "(<>) is commutative" operationCommutativity
    ]
  where
    operationAssocativity :: (MutualExculsionSet Word, MutualExculsionSet Word, MutualExculsionSet Word) -> Property
    operationAssocativity (a, b, c) = a <> (b <> c) === (a <> b) <> c

    operationCommutativity :: (MutualExculsionSet Word, MutualExculsionSet Word) -> Property
    operationCommutativity (a, b) = a <> b === b <> a


monoidProperties :: TestTree
monoidProperties = testGroup "Properties of this semigroup operator"
    [ testProperty "left identity" leftIdentity
    , testProperty "right identity" rightIdentity
    ]
  where
    leftIdentity :: MutualExculsionSet Word -> Property
    leftIdentity a = mempty <> a === a

    rightIdentity :: MutualExculsionSet Word -> Property
    rightIdentity a = a <> mempty === a


structuralProperties :: TestTree
structuralProperties = testGroup "data-structure invariants"
    [ testProperty "invert . invert === id" inversionIdentity
    , testProperty "isIncluded ==> not . isExcluded" inclusionImpliesNotExcluded
    , testProperty "isExcluded ==> not . isIncluded" exclusionImpliesNotIncluded
    , testProperty "isIncluded e === isExcluded e . invert" inclusionInvertedExclusion
    , testProperty "isExcluded e === isIncluded e . invert" exclusionInvertedInclusion
    , testProperty "includedSet === excludedSet . invert" inclusionSetInvertedExclusionSet
    , testProperty "excludedSet === includedSet . invert" exclusionSetInvertedInclusionSet
    , testProperty "toList === toList . includedSet" inclusionSetIsFoldableList
    , testProperty "toList . invert === toList . excludedSet" exclusionSetIsInvertedFoldableList
    , testProperty "∀ e, ∃ k, S.T. isIncluded e ==> excludedLookup k == Just e" includedImpliesExistsExcludedValue
    , testProperty "∀ e, ∃ k, S.T. isExcluded e ==> includedLookup k == Just e" excludedImpliesExistsIncludedValue
    ]
  where
    inversionIdentity :: MutualExculsionSet Word -> Property
    inversionIdentity mes =
        invert (invert mes) === mes

    inclusionImpliesNotExcluded :: (MutualExculsionSet Word, Word) -> Property
    inclusionImpliesNotExcluded (mes, e) =
        e `isIncluded` mes ==> not (e `isExcluded` mes)

    exclusionImpliesNotIncluded :: (MutualExculsionSet Word, Word) -> Property
    exclusionImpliesNotIncluded (mes, e) =
        e `isIncluded` mes ==> not (e `isExcluded` mes)

    inclusionInvertedExclusion :: (MutualExculsionSet Word, Word) -> Property
    inclusionInvertedExclusion (mes, e) =
        e `isIncluded` mes === e `isExcluded` invert mes

    exclusionInvertedInclusion :: (MutualExculsionSet Word, Word) -> Property
    exclusionInvertedInclusion (mes, e) =
        e `isExcluded` mes === e `isIncluded` invert mes

    inclusionSetInvertedExclusionSet :: MutualExculsionSet Word -> Property
    inclusionSetInvertedExclusionSet mes =
        includedSet mes === excludedSet (invert mes)

    exclusionSetInvertedInclusionSet :: MutualExculsionSet Word -> Property
    exclusionSetInvertedInclusionSet mes =
        excludedSet mes === includedSet (invert mes)

    inclusionSetIsFoldableList :: MutualExculsionSet Word -> Property
    inclusionSetIsFoldableList mes =
        toList mes  === toList (includedSet mes)

    exclusionSetIsInvertedFoldableList :: MutualExculsionSet Word -> Property
    exclusionSetIsInvertedFoldableList mes =
        toList (invert mes)  === toList (excludedSet mes)

    includedImpliesExistsExcludedValue :: (MutualExculsionSet Word, Word) -> Property
    includedImpliesExistsExcludedValue (mes, e) =
        e `isIncluded` mes  ==> getAny (foldMap f (excludedSet mes))
      where
        f k =
          case k `excludedLookup` mes of
            Nothing -> Any False
            Just v  -> Any $ v == e

    excludedImpliesExistsIncludedValue :: (MutualExculsionSet Word, Word) -> Property
    excludedImpliesExistsIncludedValue (mes, e) =
        e `isExcluded` mes ==> getAny (foldMap f (includedSet mes))
      where
        f k =
          case k `includedLookup` mes of
            Nothing -> Any False
            Just v  -> Any $ v == e

