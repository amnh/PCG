------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.VER.Files
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Benchmark.VER.Files
  ( verFiles
  , verFilePath
  ) where

import Data.Bits
import Data.Foldable
import System.FilePath.Posix


-- |
-- List of all benchmarking input files for the VER file parser.
verFiles :: [FilePath]
verFiles = normalizeFilePath <$> fold [networkFileNames, treeFileNames]


-- |
-- File path where benchmarking input files are located for the VER file parser.
verFilePath :: FilePath
verFilePath =  "bench" </> "data-sets" </> "ver"


normalizeFilePath :: FilePath -> FilePath
normalizeFilePath file = verFilePath </> file <.> "ver"


networkFileNames :: [FilePath]
networkFileNames = (<> "-root-network") . show <$> [3 .. 7 :: Word]


treeFileNames :: [FilePath]
treeFileNames = (<> "-leaf-tree") . show . (bit :: Int -> Word) <$> [2..12]
