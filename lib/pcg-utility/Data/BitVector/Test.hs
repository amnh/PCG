{-# LANGUAGE FlexibleInstances #-}

module Data.BitVector.Test
  ( testSuite
  ) where


--import Control.Arrow
import Data.BitVector.Normal
--import Data.Foldable
import Data.Semigroup
--import Data.MutualExclusionSet.Internal
--import Data.Word
import Test.QuickCheck
import Test.Tasty
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


{-
-- |
-- An arbitrary 'MutualExclusionSet' and a value that is included 20% likely in
-- the set.
newtype ProbablyIncluded a = PI { getProbablyIncluded :: (MutualExclusionSet a, a) }
    deriving (Eq, Ord, Show)


instance (Arbitrary a, Ord a) => Arbitrary (ProbablyIncluded a) where

    arbitrary = do
        mes <- arbitrary
        inc <- case toList $ includedSet mes of
               [] -> arbitrary
               xs -> frequency [ (1, arbitrary), (4, elements xs) ]
        pure $ PI (mes, inc)
-}


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ orderingProperties
    , semigroupProperties
    , monoidProperties
--    , constructionCases
--    , structuralProperties
    ]


{-
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
      singleton 1 2  <> singleton 2 1 @?= unsafeFromList [ (1, 2), (2, 1 :: Word8) ]
    
    paradoxPermissible = lhs <> rhs @?= unsafeFromList [ (1, 2), (2, 1), (3, 4), (5, 6 :: Word8) ]
      where
        lhs = singleton 1 2 <> singleton 3 4 :: BitVector
        rhs = singleton 2 1 <> singleton 5 6 :: BitVector

{-
    inclusionSetViolation =
      (singleton 1 2 <> singleton 1 3) @?= unsafeFromList [ (1, 3) ]

    exclusionSetViolation =
      (singleton 1 3 <> singleton 2 3) @?= unsafeFromList [ (2, 3) ]
-}
-}


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of ordering"
    [ testProperty "ordering preserves symetry" symetry
    , testProperty "ordering is transitive" transitive
    ]
  where
    symetry :: (BitVector, BitVector) -> Bool
    symetry (lhs, rhs) =
      case (lhs `compare` rhs, rhs `compare` lhs) of
        (EQ, EQ) -> True
        (GT, LT) -> True
        (LT, GT) -> True
        _        -> False

    transitive :: (BitVector, BitVector, BitVector) -> Property
    transitive (a, b, c) = ab == bc ==> ab === ac
      where
        ab = a `compare` b
        bc = b `compare` c
        ac = a `compare` c

semigroupProperties :: TestTree
semigroupProperties = testGroup "Properties of this semigroup operator"
    [ localOption (QuickCheckTests 10000)
        $ testProperty "(<>) is associative" operationAssocativity
    ]
  where
    operationAssocativity :: (BitVector, BitVector, BitVector) -> Property
    operationAssocativity (a, b, c) = a <> (b <> c) === (a <> b) <> c


monoidProperties :: TestTree
monoidProperties = testGroup "Properties of this semigroup operator"
    [ testProperty "left identity" leftIdentity
    , testProperty "right identity" rightIdentity
    ]
  where
    leftIdentity :: BitVector -> Property
    leftIdentity a = mempty <> a === a

    rightIdentity :: BitVector -> Property
    rightIdentity a = a <> mempty === a


{-
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
    inversionIdentity :: BitVector -> Property
    inversionIdentity mes =
        invert (invert mes) === mes

    inclusionImpliesNotExcluded :: ProbablyIncluded Word8 -> Property
    inclusionImpliesNotExcluded input =
        e `isIncluded` mes ==> not (e `isExcluded` mes)
      where
        (mes, e) = getProbablyIncluded input

    exclusionImpliesNotIncluded :: ProbablyIncluded Word8 -> Property
    exclusionImpliesNotIncluded input =
        e `isIncluded` mes ==> not (e `isExcluded` mes)
      where
        (mes, e) = getProbablyIncluded input

    inclusionInvertedExclusion :: (BitVector, Word8) -> Property
    inclusionInvertedExclusion (mes, e) =
        e `isIncluded` mes === e `isExcluded` invert mes

    exclusionInvertedInclusion :: (BitVector, Word8) -> Property
    exclusionInvertedInclusion (mes, e) =
        e `isExcluded` mes === e `isIncluded` invert mes

    inclusionSetInvertedExclusionSet :: BitVector -> Property
    inclusionSetInvertedExclusionSet mes =
        includedSet mes === excludedSet (invert mes)

    exclusionSetInvertedInclusionSet :: BitVector -> Property
    exclusionSetInvertedInclusionSet mes =
        excludedSet mes === includedSet (invert mes)

    inclusionSetIsFoldableList :: BitVector -> Property
    inclusionSetIsFoldableList mes =
        toList mes  === toList (includedSet mes)

    exclusionSetIsInvertedFoldableList :: BitVector -> Property
    exclusionSetIsInvertedFoldableList mes =
        toList (invert mes) === toList (excludedSet mes)

    includedImpliesExistsExcludedValue :: ProbablyIncluded Word8 -> Property
    includedImpliesExistsExcludedValue input =
        e `isIncluded` mes  ==> getAny (foldMap f (excludedSet mes))
      where
        (mes, e) = getProbablyIncluded input
        f k =
            case k `excludedLookup` mes of
              Nothing -> Any False
              Just v  -> Any $ v == e
        
    excludedImpliesExistsIncludedValue :: ProbablyIncluded Word8 -> Property
    excludedImpliesExistsIncludedValue input =
         e `isExcluded` mes ==> getAny (foldMap f (includedSet mes))
      where
        (mes, e) = first invert $ getProbablyIncluded input
        f k =
            case k `includedLookup` mes of
              Nothing -> Any False
              Just v -> Any $ v == e      
-}
