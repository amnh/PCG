{-# LANGUAGE BangPatterns #-}

module Data.MutualExclusionSet.Bench (benchmarks) where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.MutualExclusionSet.Internal
import Data.Semigroup


benchmarks :: Benchmark
benchmarks = bgroup "MutualExclusionSet"
    [ singletonBench
    , invertBench
    , isCoherentBench
    , isExcludedBench 
    , isIncludedBench
    , excludedLookupBench 
    , includedLookupBench
    , excludedSetBench
    , includedSetBench
    , mutualExclusivePairsBench
    , mergeBench
    , isPermissibleBench
    ]


singletonBench :: Benchmark
singletonBench = bench "MutualExclusionSet singleton is constant-time construction" . nf (singleton 42) $ (1 :: Int)


invertBench :: Benchmark
invertBench = bench "MutualExclusionSet invert is constant-time" $ whnf invert $ force (ofSize 50)


isCoherentBench :: Benchmark
isCoherentBench = bench "MutualExclusionSet isCoherent is constant-time" $ whnf isCoherent $ force (ofSize 50)


isPermissibleBench :: Benchmark
isPermissibleBench = linearBenchmark "MutualExclusionSet isPermissible log-access" (force . ofSize) (const isPermissible)


isExcludedBench :: Benchmark
isExcludedBench = logBenchmark "MutualExclusionSet isExcluded log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `isExcluded` xs) `seq` (negate i) `isExcluded` xs


isIncludedBench :: Benchmark
isIncludedBench = logBenchmark "MutualExclusionSet isIncluded log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `isIncluded` xs) `seq` (negate i) `isIncluded` xs


excludedLookupBench :: Benchmark
excludedLookupBench = logBenchmark "MutualExclusionSet excludedLookup log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `excludedLookup` xs) `seq` (negate i) `excludedLookup` xs


includedLookupBench :: Benchmark
includedLookupBench = logBenchmark "MutualExclusionSet includedLookup log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `includedLookup` xs) `seq` (negate i) `includedLookup` xs


excludedSetBench :: Benchmark
excludedSetBench = linearBenchmark "MutualExclusionSet excludedSet linear access" (force . ofSize) (const excludedSet)


includedSetBench :: Benchmark
includedSetBench = linearBenchmark "MutualExclusionSet includedSet linear access" (force . ofSize) (const includedSet)


mutualExclusivePairsBench :: Benchmark
mutualExclusivePairsBench = linearBenchmark "MutualExclusionSet mutuallyExclusivePairs linear access" (force . ofSize) (const mutuallyExclusivePairs)


mergeBench :: Benchmark
mergeBench = linearBenchmark2 "merge (<>) is linear" (force . ofSize) (force . ofSize) (const (<>))


linearBenchmark :: (NFData a, NFData b) => String -> (Int -> a) -> (Int -> a -> b) -> Benchmark
linearBenchmark  label f g = bgroup label $ generateBenchmark <$> [0 .. 9]
  where
    generateBenchmark exp = bench (show domainSize) $ nf app target
      where
        !target    = force $ f domainSize
        !app       = g exp
        domainSize = 10 * (exp + 1)
    

linearBenchmark2 :: (NFData a, NFData b, NFData c) => String -> (Int -> a) -> (Int -> b) -> (Int -> a -> b -> c) -> Benchmark
linearBenchmark2  label f g h = bgroup label $ generateBenchmark <$> [0 .. 9]
  where
    generateBenchmark exp = bench (show domainSize) $ nf app rhs
      where
        !lhs       = force $ f domainSize
        !rhs       = force $ g domainSize
        !app       = h exp lhs
        domainSize = 10 * (exp + 1)
    

logBenchmark :: String -> (Int -> a) -> (Int -> a -> b) -> Benchmark
logBenchmark label f g = bgroup label $ generateBenchmark <$> [0 .. 9]
  where
    generateBenchmark exp = bench (show domainSize) $ whnf app target
      where
        !app       = g indexProd
        !target    = f domainSize
        indexProd  = product [1..exp] `mod` domainSize
        domainSize = 2 `shiftL` exp


ofSize :: Int -> MutualExclusionSet Int
ofSize n = unsafeFromList $ (\x -> (x, negate x)) <$> [1..n]


ofSizeEven :: Int -> MutualExclusionSet Int
ofSizeEven n = unsafeFromList $ (\x -> (x, negate x)) <$> [2,4..2*n]
