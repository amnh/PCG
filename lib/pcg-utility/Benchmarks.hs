
module Main (main) where


import           Criterion.Main
import qualified Data.MutualExclusionSet.Bench  as MutualExclusionSet
import qualified Numeric.Extended.Natural.Bench as ExtendedNatural


main :: IO ()
main = defaultMain
    [ MutualExclusionSet.benchmarks
    , ExtendedNatural.benchmarks
    ]
