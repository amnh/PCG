{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.MutualExclusionSet.Internal
import Data.Semigroup


main :: IO ()
main = defaultMain
    [ isExcludedBench 
    , isIncludedBench
    , excludedSetBench
    , includedSetBench
    , mutualExclusivePairsBench
    , mergeBench
    ]


firstBench :: Benchmark
firstBench = bgroup "MutualExclusiveSet access" 
    [ bench     "1" . whnf (includedLookup   1) $ ofSize     1
    , bench     "2" . whnf (includedLookup   2) $ ofSize     2
    , bench     "4" . whnf (includedLookup   3) $ ofSize     4
    , bench     "8" . whnf (includedLookup   5) $ ofSize     8
    , bench    "16" . whnf (includedLookup   9) $ ofSize    16
    , bench    "32" . whnf (includedLookup   7) $ ofSize    32
    , bench    "64" . whnf (includedLookup  47) $ ofSize    64
    , bench   "128" . whnf (includedLookup  29) $ ofSize   128
    , bench   "265" . whnf (includedLookup 127) $ ofSize   256
    , bench   "512" . whnf (includedLookup  67) $ ofSize   512
    , bench  "1024" . whnf (includedLookup 789) $ ofSize  1024
    , bench  "2048" . whnf (includedLookup  67) $ ofSize  2048
    , bench  "4096" . whnf (includedLookup 789) $ ofSize  4096
    , bench  "8192" . whnf (includedLookup 789) $ ofSize  8192
    , bench "16384" . whnf (includedLookup  67) $ ofSize 16384
    , bench "32768" . whnf (includedLookup 789) $ ofSize 32768
    , bench "65536" . whnf (includedLookup 789) $ ofSize 65536
    ]


isExcludedBench :: Benchmark
isExcludedBench = logBenchmark "MutualExclusiveSet isExcluded log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `excludedLookup` xs) `seq` (negate i) `excludedLookup` xs


isIncludedBench :: Benchmark
isIncludedBench = logBenchmark "MutualExclusiveSet isIncluded log-access" ofSize f
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `includedLookup` xs) `seq` (negate i) `includedLookup` xs


excludedSetBench :: Benchmark
excludedSetBench = linearBenchmark "MutualExclusiveSet excludedSet linear access" (force . ofSize) (const excludedSet)


includedSetBench :: Benchmark
includedSetBench = linearBenchmark "MutualExclusiveSet includedSet linear access" (force . ofSize) (const includedSet)


mutualExclusivePairsBench :: Benchmark
mutualExclusivePairsBench = linearBenchmark "MutualExclusiveSet includedSet linear access" (force . ofSize) (const includedSet)


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
logBenchmark label f g = bgroup label $ generateBenchmark <$> [0 .. 17]
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
