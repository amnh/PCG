------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.FASTA.Files
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Benchmark.FASTA.Files
  ( fastaFilePath
  , fastaInlineSequenceFiles
  ) where

import Data.Foldable
import System.FilePath.Posix


-- |
-- List of all benchmarking input files for the FASTA file parser.
fastaInlineSequenceFiles :: [FilePath]
fastaInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    sequenceLength <- sequenceLengths
    let fileName = fold [ show taxaCount, "x", show sequenceLength, ".fasta" ]
    pure $ fastaFilePath </> fileName


-- |
-- File path where benchmarking input files are located for the FASTA file parser.
fastaFilePath :: FilePath
fastaFilePath =  "bench" </> "data-sets" </>"fasta"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 5 :: Word]


sequenceLengths :: [Word]
sequenceLengths = (8 *) . (2^) <$> [0 .. 9 :: Word]
