------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main (main) where

import qualified Benchmark.FASTA.Space  as FASTA
import qualified Benchmark.FASTC.Space  as FASTC
import qualified Benchmark.Newick.Space as Newick
import qualified Benchmark.TCM.Space    as TCM
import qualified Benchmark.VER.Space    as VER
import           Data.Foldable
import           Weigh


-- |
-- Entry point for the space allocation performance benchmark suite /all/ the file parsers.
main :: IO ()
main = mainWith $ do
    setColumns [Case, Allocated, GCs, Max]
    sequenceA_ $ fold
      [ FASTA.benchSpace
      , FASTC.benchSpace
      , Newick.benchSpace
      , TCM.benchSpace
      , VER.benchSpace
      ]
