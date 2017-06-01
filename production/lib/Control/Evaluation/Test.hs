{-# ANN module "HLint: error Functor law" #-}

{-# LANGUAGE AllowAmbiguousTypes, ViewPatterns #-}

module Control.Evaluation.Test where

import Control.Evaluation.Unit
import Control.Evaluation.Internal
--import PCG.Evaluation.Trans
import Data.Monoid
import Test.Tasty                 (TestTree, testGroup)
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function


testSuite :: TestTree
testSuite = testGroup "Evaluation Monad"
    [ evalUnitLaws
    , evaluationLaws
    ]


evalUnitLaws :: TestTree
evalUnitLaws = testGroup "EvalUnit" [functorLaws, applicativeLaws, monadLaws, monoidLaws]
  where
    functorLaws = testGroup "Functor Laws" [functorIdentity', functorComposition']
      where
        functorIdentity'        = testGroup "Identity"       [testProperty "fmap id  ==  id"                   (functorIdentity    :: EvalUnit Int -> Bool)]
        functorComposition'     = testGroup "Composition"    [testProperty "fmap (f . g)  ==  fmap f . fmap g" (functorComposition :: EvalUnit Int -> Fun Int Int -> Fun Int Int -> Bool)]

    applicativeLaws = testGroup "Applicative Laws" [applicativeIdentity', applicativeComposition', applicativeInterchange']
      where
        applicativeIdentity'    = testGroup "Identity"       [testProperty "pure id <*> v  ==  v"                            (applicativeIdentity    :: EvalUnit Int -> Bool)]
        applicativeComposition' = testGroup "Composition"    [testProperty "pure (.) <*> u <*> v <*> w  ==  u <*> (v <*> w)" (applicativeComposition :: Blind (EvalUnit (Int -> Int)) -> Blind (EvalUnit (Int -> Int)) -> EvalUnit Int -> Bool)]
        applicativeInterchange' = testGroup "Interchange"    [testProperty "u <*> pure y  ==  pure ($ y) <*> u"              (applicativeInterchange :: Blind (EvalUnit (Int -> Int)) -> Int -> Bool)]

    monadLaws :: TestTree
    monadLaws = testGroup "Monad Laws" [monadLeftIdentity', monadRightIdentity', monadAssociativity']
      where
        monadLeftIdentity'     = testGroup "Left Identity"   [testProperty "return a >>= k  ==  k a"                     (monadLeftIdentity  :: Int -> Fun Int (EvalUnit Int) -> Bool)]
        monadRightIdentity'    = testGroup "Right Identity"  [testProperty "m >>= return  ==  m"                         (monadRightIdentity :: EvalUnit Int -> Bool)]
        monadAssociativity'    = testGroup "Associativity"   [testProperty "m >>= (x -> k x >>= h)  ==  (m >>= k) >>= h" (monadAssociativity :: EvalUnit Int -> Fun Int (EvalUnit Int) -> Fun Int (EvalUnit Int) -> Bool)]

    monoidLaws :: TestTree
    monoidLaws = testGroup "Monoid Laws" [monoidLeftIdentity', monoidRightIdentity', monoidAssociativity']
      where
        monoidLeftIdentity'     = testGroup "Left Identity"  [testProperty "(mempty <> x)  ==  x"                 (monoidLeftIdentity  :: EvalUnit Int -> Bool)]
        monoidRightIdentity'    = testGroup "Right Identity" [testProperty "x == (x <> mempty)"                   (monoidRightIdentity :: EvalUnit Int -> Bool)]
        monoidAssociativity'    = testGroup "Associativity"  [testProperty "(x <> (y <> z))  ==  ((x <> y) <> z)" (monoidAssociativity :: EvalUnit Int -> EvalUnit Int -> EvalUnit Int -> Bool)]


evaluationLaws :: TestTree
evaluationLaws = testGroup "Evaluation" [functorLaws, applicativeLaws, monadLaws, monoidLaws]
  where
    functorLaws = testGroup "Functor Laws" [functorIdentity', functorComposition']
      where
        functorIdentity'        = testGroup "Identity"       [testProperty "fmap id  ==  id"                   (functorIdentity    :: Evaluation Int -> Bool)]
        functorComposition'     = testGroup "Composition"    [testProperty "fmap (f . g)  ==  fmap f . fmap g" (functorComposition :: Evaluation Int -> Fun Int Int -> Fun Int Int -> Bool)]

    applicativeLaws = testGroup "Applicative Laws" [applicativeIdentity', applicativeComposition', applicativeInterchange']
      where
        applicativeIdentity'    = testGroup "Identity"       [testProperty "pure id <*> v  ==  v"                            (applicativeIdentity    :: Evaluation Int -> Bool)]
        applicativeComposition' = testGroup "Composition"    [testProperty "pure (.) <*> u <*> v <*> w  ==  u <*> (v <*> w)" (applicativeComposition :: Blind (Evaluation (Int -> Int)) -> Blind (Evaluation (Int -> Int)) -> Evaluation Int -> Bool)]
        applicativeInterchange' = testGroup "Interchange"    [testProperty "u <*> pure y  ==  pure ($ y) <*> u"              (applicativeInterchange :: Blind (Evaluation (Int -> Int)) -> Int -> Bool)]

    monadLaws :: TestTree
    monadLaws = testGroup "Monad Laws" [monadLeftIdentity', monadRightIdentity', monadAssociativity']
      where
        monadLeftIdentity'     = testGroup "Left Identity"   [testProperty "return a >>= k  ==  k a"                     (monadLeftIdentity  :: Int -> Fun Int (Evaluation Int) -> Bool)]
        monadRightIdentity'    = testGroup "Right Identity"  [testProperty "m >>= return  ==  m"                         (monadRightIdentity :: Evaluation Int -> Bool)]
        monadAssociativity'    = testGroup "Associativity"   [testProperty "m >>= (x -> k x >>= h)  ==  (m >>= k) >>= h" (monadAssociativity :: Evaluation Int -> Fun Int (Evaluation Int) -> Fun Int (Evaluation Int) -> Bool)]

    monoidLaws :: TestTree
    monoidLaws = testGroup "Monoid Laws" [monoidLeftIdentity', monoidRightIdentity', monoidAssociativity']
      where
        monoidLeftIdentity'     = testGroup "Left Identity"  [testProperty "(mempty <> x)  ==  x"                 (monoidLeftIdentity  :: Evaluation Int -> Bool)]
        monoidRightIdentity'    = testGroup "Right Identity" [testProperty "x == (x <> mempty)"                   (monoidRightIdentity :: Evaluation Int -> Bool)]
        monoidAssociativity'    = testGroup "Associativity"  [testProperty "(x <> (y <> z))  ==  ((x <> y) <> z)" (monoidAssociativity :: Evaluation Int -> Evaluation Int -> Evaluation Int -> Bool)]




functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x


functorComposition :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorComposition x (apply -> f) (apply -> g) = fmap (g . f) x == (fmap g . fmap f $ x)


applicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
applicativeIdentity x = (pure id <*> x) == x


applicativeComposition :: (Applicative f, Eq (f a), Eq (f c)) => Blind (f (b -> c)) -> Blind (f (a -> b)) -> f a -> Bool
applicativeComposition x' y' z = (pure (.) <*> x <*> y <*> z) == (x <*> (y <*> z))
  where
    x = getBlind x'
    y = getBlind y'


{- | Cant figure the test out :(
applicativeHomomorphism :: (Applicative f, Eq (f b)) => a -> Fun a b -> Bool  
applicativeHomomorphism x (apply -> f) = (pure f <*> pure x) == pure (f x)
-}


applicativeInterchange :: (Applicative f, Eq (f b)) => Blind (f (a -> b)) -> a -> Bool
applicativeInterchange x' y = (x <*> pure y) == (pure ($ y) <*> x)
  where
    x = getBlind x'


monadRightIdentity :: (Monad m, Eq (m a)) => m a -> Bool
monadRightIdentity x = (x >>= pure) == x


monadLeftIdentity :: (Monad m, Eq (m b)) => a -> Fun a (m b) -> Bool
monadLeftIdentity x (apply -> f) = (pure x >>= f) == f x


monadAssociativity :: (Monad m, Eq (m c)) => m a -> Fun a (m b) -> Fun b (m c) -> Bool
monadAssociativity x (apply -> f) (apply -> g) = ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = x == (x <> mempty)


monoidAssociativity :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssociativity x y z = (x <> (y <> z)) == ((x <> y) <> z)
