------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.FASTC.Files
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Benchmark.FASTC.Files
  ( fastcFilePath
  , fastcSequenceFiles
  ) where

import Data.Foldable
import System.FilePath.Posix


-- |
-- List of all benchmarking input files for the FASTC file parser.
fastcSequenceFiles :: [FilePath]
fastcSequenceFiles = do
    taxaCount      <- taxaCounts
    sequenceLength <- sequenceLengths
    symbolSize     <- symbolSizes
    let fileName = fold [ show taxaCount, "x", show sequenceLength, "+", show symbolSize, ".fastc" ]
    pure $ fastcFilePath </> fileName


-- |
-- File path where benchmarking input files are located for the FASTC file parser.
fastcFilePath :: FilePath
fastcFilePath =  "bench" </> "data-sets" </>"fastc"


symbolSizes :: [Word]
symbolSizes = (2 *) . (2^) <$> [0 .. 3 :: Word]


taxaCounts :: [Word]
taxaCounts = (4 *) . (4^) <$> [0 .. 3 :: Word]


sequenceLengths :: [Word]
sequenceLengths = (16 *) . (4^) <$> [0 .. 3 :: Word]

