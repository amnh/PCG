{-# LANGUAGE FlexibleInstances #-}

module Control.Parallel.Test
  ( testSuite
  ) where

import Control.Parallel.Custom
import Control.Parallel.Strategies
import Test.Tasty
import Test.Tasty.QuickCheck


testSuite :: TestTree
testSuite = testGroup "Custom Parallel tests"
    [ parallelEquivalencyProperties
    ]


parallelEquivalencyProperties :: TestTree
parallelEquivalencyProperties = testGroup "Parallel Equivalency"
    [ testProperty "parmap rpar f === fmap f" parmapEquivalency
    , testProperty "parZipWith rpar f x === zipWith f x" parZipWithEquivalency
    , testProperty "parZipWith3 rpar f x y === zipWith3 f x y" parZipWith3Equivalency
    ]
  where
    parmapEquivalency :: Blind (Word -> String) -> [Word] -> Property
    parmapEquivalency (Blind f) xs =
        parmap rpar f xs === fmap f xs

    parZipWithEquivalency :: Blind (Word -> Double -> String) -> [Word] -> [Double] -> Property
    parZipWithEquivalency (Blind f) x y =
        parZipWith rpar f x y === zipWith f x y

    parZipWith3Equivalency :: Blind (Word -> Double -> Char -> String) -> [Word] -> [Double] -> String -> Property
    parZipWith3Equivalency (Blind f) x y z =
        parZipWith3 rpar f x y z === zipWith3 f x y z
