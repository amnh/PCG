------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.TCM.Files
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Benchmark.TCM.Files
  ( tcmFiles
  , tcmFilePath
  ) where

import System.FilePath.Posix


-- |
-- List of all benchmarking input files for the tcm file parser.
tcmFiles :: [FilePath]
tcmFiles = (\file -> tcmFilePath </> file <.> "tcm") <$> fileNames


-- |
-- File path where benchmarking input files are located for the TCM file parser.
tcmFilePath :: FilePath
tcmFilePath =  "bench" </> "data-sets" </> "tcm"


fileNames :: [FilePath]
fileNames =
    [ "dna-1-2"
    , "dna-2-1"
    , "dna-discrete"
    , "dna-g2t2"
    , "dna-g4t4"
    , "dna-L1-norm"
    , "huge-mix-1-2"
    , "huge-mix-2-1"
    , "huge-mix-discrete"
    , "huge-mix-hamming"
    , "huge-mix-L1-norm"
    , "huge-mix-leveshtein"
    , "large-mix-1-2"
    , "large-mix-2-1"
    , "large-mix-discrete"
    , "large-mix-hamming"
    , "large-mix-L1-norm"
    , "large-mix-leveshtein"
    , "protein-1-2-L1-norm"
    , "protein-1-2"
    , "protein-2-1"
    , "protein-discrete"
    , "protein-L1-norm"
    , "slashes-1-2"
    , "slashes-2-1"
    , "slashes-discrete"
    , "slashes-hamming"
    , "slashes-L1-norm"
    , "slashes-leveshtein"
    , "wag-1-2"
    , "wag-1-ca"
    ]
