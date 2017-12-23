module Main (main) where


import           Criterion.Main
import qualified Data.BitVector.Bench           as BitVector
import qualified Data.MutualExclusionSet.Bench  as MutualExclusionSet
import qualified Numeric.Extended.Natural.Bench as ExtendedNatural


main :: IO ()
main = defaultMain
    [ BitVector.benchmarks
--    , MutualExclusionSet.benchmarks
--    , ExtendedNatural.benchmarks
    ]
