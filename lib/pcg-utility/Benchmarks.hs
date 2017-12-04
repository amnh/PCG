{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.MutualExclusionSet.Internal


main :: IO ()
main = defaultMain
    [ isIncludedBench
    , isExcludedBench
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
isIncludedBench = logBenchmark "MutualExclusiveSet isIncluded log-access" ofSize
  where
    -- We negate i to consider both the included and excluded cases
    f i xs = (i `includedLookup` xs) `seq` (negate i) `includedLookup` xs



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
