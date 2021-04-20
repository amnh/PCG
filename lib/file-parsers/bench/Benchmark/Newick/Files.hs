------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.Newick.Files
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Benchmark.Newick.Files
  ( newickFilePath
  , newickInlineSequenceFiles
  ) where

import Data.Foldable
import System.FilePath.Posix


-- |
-- List of all benchmarking input files for the eNewick file parser.
newickInlineSequenceFiles :: [FilePath]
newickInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    let fileName = fold [ show taxaCount, ".tree" ]
    pure $ newickFilePath </> fileName


-- |
-- File path where benchmarking input files are located for the eNewick file parser.
newickFilePath :: FilePath
newickFilePath =  "bench" </> "data-sets" </>"newick"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 9 :: Word]
