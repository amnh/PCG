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

import qualified Benchmark.FASTA.Time  as FASTA
import qualified Benchmark.FASTC.Time  as FASTC
import qualified Benchmark.Newick.Time as Newick
import qualified Benchmark.TCM.Time    as TCM
import qualified Benchmark.VER.Time    as VER
import           Criterion.Main
import           Data.Foldable


-- |
-- Entry point for the run time performance benchmark suite /all/ the file parsers.
main :: IO ()
main = defaultMain $ fold
    [ FASTA.benchTime
    , FASTC.benchTime
    , Newick.benchTime
    , TCM.benchTime
    , VER.benchTime
    ]
