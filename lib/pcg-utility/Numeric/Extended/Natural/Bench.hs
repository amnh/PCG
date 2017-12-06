{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Numeric.Extended.Natural.Bench (benchmarks) where

import Control.DeepSeq
import Criterion.Main
import Data.Semigroup
import Numeric.Extended.Natural


benchmarks :: Benchmark
benchmarks = bgroup "ExtendedNatural" $
    [ fromProjectionBench
    , toProjectionBench
    ] <> interlace wordBench extendedNaturalBench


wordBench :: [Benchmark]
wordBench =
    [ binaryOperationBenchmark "Word addition"         "+"    (+)
    , binaryOperationBenchmark "Word subtraction"      "-"    (-)
    , binaryOperationBenchmark "Word multiplication"   "*"    (*)
    , binaryOperationBenchmark "Word division"         "/"    div
    , unaryOperationBenchmark  "Word absolute value" "abs"    abs
    , unaryOperationBenchmark  "Word sign value"  "signum" signum
    , unaryOperationBenchmark  "Word negation"    "negate" negate
    ] <*> [ wordValues ]


extendedNaturalBench :: [Benchmark]
extendedNaturalBench =
    [ binaryOperationBenchmark "ExtendedNatural addition"         "+"    (+)
    , binaryOperationBenchmark "ExtendedNatural subtraction"      "-"    (-)
    , binaryOperationBenchmark "ExtendedNatural multiplication"   "*"    (*)
    , binaryOperationBenchmark "ExtendedNatural division"         "/"    div
    , unaryOperationBenchmark  "ExtendedNatural absolute value" "abs"    abs
    , unaryOperationBenchmark  "ExtendedNatural sign value"  "signum" signum
    , unaryOperationBenchmark  "ExtendedNatural negation"    "negate" negate
    ] <*> [ extendedNaturalValues ]


fromProjectionBench :: Benchmark
fromProjectionBench = bgroup "ExtendedNatural fromFinite" $ generateBenchmark <$> wordValues
  where
    generateBenchmark x = bench (unwords ["fromFinite", show x]) $ nf operation x
    operation = fromFinite :: Word -> ExtendedNatural


toProjectionBench :: Benchmark
toProjectionBench = bgroup "ExtendedNatural unsafeToFinite" $ generateBenchmark <$> extendedNaturalValues
  where
    generateBenchmark x = bench (unwords ["unsafeToFinite", show x]) $ nf unsafeToFinite x


binaryOperationBenchmark :: (NFData a, Ord a, Show a) => String -> String -> (a -> a -> a) -> [a] -> Benchmark
binaryOperationBenchmark label str op values = bgroup label $ generateBenchmark <$> valuePairs
  where
    generateBenchmark (x, y) = bench (unwords [show x, str, show y]) $ nf (op x) y
    valuePairs = [ (x, y) | x <- values, y <- values, x < y ]


unaryOperationBenchmark :: (NFData a, Show a) => String -> String -> (a -> a) -> [a] -> Benchmark
unaryOperationBenchmark label str op values = bgroup label $ generateBenchmark <$> values
  where
    generateBenchmark x = bench (unwords [str, show x]) $ nf op x


extendedNaturalValues :: [ExtendedNatural]
extendedNaturalValues = [ infinity, maxBound, minBound ] <> (tenToThe <$> [ 1, 6, 11, 16 ])


wordValues :: [Word]
wordValues = [ maxBound, maxBound - 1, minBound ] <> (tenToThe <$> [ 1, 6, 11, 16 ])


tenToThe :: Num a => Word -> a
tenToThe = (10^)


interlace :: [a] -> [a] -> [a]
interlace    []     []  = []
interlace    []     ys  = ys
interlace    xs     []  = xs
interlace (x:xs) (y:ys) = x : y : interlace xs ys
