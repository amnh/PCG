module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Bits
import Data.MutualExclusionSet.Internal


main = defaultMain
    [ firstBench
    , firstBench2
    ]


firstBench = bgroup "MutualExclusiveSet access" 
    [ bench    "1" . whnf (includedLookup   1) $ ofSize    1
    , bench    "2" . whnf (includedLookup   2) $ ofSize    2
    , bench    "4" . whnf (includedLookup   3) $ ofSize    4
    , bench    "8" . whnf (includedLookup   5) $ ofSize    8
    , bench   "16" . whnf (includedLookup   9) $ ofSize   16
    , bench   "32" . whnf (includedLookup   7) $ ofSize   32
    , bench   "64" . whnf (includedLookup  47) $ ofSize   64
    , bench  "128" . whnf (includedLookup  29) $ ofSize  128
    , bench  "265" . whnf (includedLookup 127) $ ofSize  256
    , bench  "512" . whnf (includedLookup  67) $ ofSize  512
    , bench "1024" . whnf (includedLookup 789) $ ofSize 1024
    ]


firstBench2 = logBenchmark "MutualExclusiveSet isIncluded log-access" ofSize f
  where
    f :: Int -> MutualExclusionSet Int -> Maybe Int
    f exp = includedLookup idx
      where
        idx = 5 * (2 `shiftL` (exp - 1)) `mod` (2 `shiftL` exp)
                           


logBenchmark :: String -> (Int -> a) -> (Int -> a -> b) -> Benchmark
logBenchmark label f g = bgroup label $ generateBenchmark <$> [0 .. 15]
  where
    generateBenchmark exp = bench (show domainSize) . whnf (g exp) $ force f domainSize
      where
        domainSize = 2 `shiftL` exp


ofSize :: Int -> MutualExclusionSet Int
ofSize n = unsafeFromList $ (\x -> (x, negate x)) <$> [1..n]
