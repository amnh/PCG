module Main (main) where

import qualified Benchmark.FASTA.Time as FASTA
import qualified Benchmark.FASTC.Time as FASTC
import           Criterion.Main
import           Data.Foldable


main :: IO ()
main = defaultMain $ fold
    [ FASTA.benchTime
    , FASTC.benchTime
    ] 
