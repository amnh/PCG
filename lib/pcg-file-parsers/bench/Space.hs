module Main (main) where

import qualified Benchmark.FASTA.Space  as FASTA
import qualified Benchmark.FASTC.Space  as FASTC
import qualified Benchmark.Newick.Space as Newick
import           Data.Foldable
import           Weigh


main :: IO ()
main = mainWith $ do
    setColumns [Case, Allocated, GCs, Max]
    sequenceA_ $ fold
      [ FASTA.benchSpace
      , FASTC.benchSpace
      , Newick.benchSpace
      ]
