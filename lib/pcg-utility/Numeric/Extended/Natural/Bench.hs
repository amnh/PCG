{-# LANGUAGE FlexibleContexts #-}

module Numeric.Extended.Natural.Bench (benchmarks) where

import Control.DeepSeq
import Criterion.Main
import Numeric.Extended.Natural


benchmarks :: Benchmark
benchmarks = bgroup "ExtendedNatural" $
    [ fromProjectionBench
    , toProjectionBench
    ] <> extendedNaturalBench


extendedNaturalBench :: [Benchmark]
extendedNaturalBench =
    [ bgroup "addition" $ interlace
        ( binaryOperationBenchmark      "+"    (+) "ExtendedNatural" extendedNaturalValues)
        $ binaryOperationBenchmark      "+"    (+) "Word"            wordValues
    , bgroup "subtraction" $ interlace
        ( binaryOperationBenchmark      "-"    (-) "ExtendedNatural" extendedNaturalValues)
        $ binaryOperationBenchmark      "-"    (-) "Word"            wordValues
    , bgroup "multiplication" $ interlace
        ( binaryOperationBenchmark      "*"    (*) "ExtendedNatural" extendedNaturalValues)
        $ binaryOperationBenchmark      "*"    (*) "Word"            wordValues
    , bgroup "division" . interlace
        ( binaryOperationBenchmark      "/"    div "ExtendedNatural" extendedNaturalValues)
        $ binaryOperationBenchmark      "/"    div "Word"            wordValues
    , bgroup "absolute value" . interlace
        ( unaryOperationBenchmark    "abs"    abs "ExtendedNatural" extendedNaturalValues)
        $ unaryOperationBenchmark    "abs"    abs "Word"            wordValues
    , bgroup "sign number" . interlace
        ( unaryOperationBenchmark "signum" signum "ExtendedNatural" extendedNaturalValues)
        $ unaryOperationBenchmark "signum" signum "Word"            wordValues
    , bgroup "negation" . interlace 
        ( unaryOperationBenchmark "negate" negate "ExtendedNatural" extendedNaturalValues)
        $ unaryOperationBenchmark "negate" negate "WordValue"       wordValues
    ]


fromProjectionBench :: Benchmark
fromProjectionBench = bgroup "ExtendedNatural fromFinite" $ generateBenchmark <$> wordValues
  where
    generateBenchmark x = bench (unwords ["fromFinite", show x]) $ nf operation x
    operation = fromFinite :: Word -> ExtendedNatural


toProjectionBench :: Benchmark
toProjectionBench = bgroup "ExtendedNatural unsafeToFinite" $ generateBenchmark <$> extendedNaturalValues
  where
    generateBenchmark x = bench (unwords ["unsafeToFinite", show x]) $ nf unsafeToFinite x


binaryOperationBenchmark :: (NFData a, Ord a, Show a) => String -> (a -> a -> a) -> String -> [a] -> [Benchmark]
binaryOperationBenchmark opStr op typeStr values = generateBenchmark typeStr <$> valuePairs
  where
    generateBenchmark z (x, y) = bench (unwords [z, show x, opStr, show y]) $ nf (op x) y
    valuePairs = [ (x, y) | x <- values, y <- values, x < y ]


unaryOperationBenchmark :: (NFData a, Show a) => String -> (a -> a) -> String -> [a] -> [Benchmark]
unaryOperationBenchmark opStr op typeStr values = generateBenchmark typeStr <$> values
  where
    generateBenchmark y x = bench (unwords [y, opStr, show x]) $ nf op x


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
