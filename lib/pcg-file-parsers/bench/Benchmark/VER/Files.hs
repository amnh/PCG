module Benchmark.VER.Files
  ( verFiles
  , verFilePath
  ) where

import Data.Bits
import Data.Foldable
import System.FilePath.Posix


verFiles :: [FilePath]
verFiles = normalizeFilePath <$> fold [networkFileNames, treeFileNames]


verFilePath :: FilePath
verFilePath =  "bench" </> "data-sets" </> "ver"


normalizeFilePath :: FilePath -> FilePath
normalizeFilePath file = verFilePath </> file <.> "ver"


networkFileNames :: [FilePath]
networkFileNames = (<> "-root-network") . show <$> [3 .. 7 :: Word]


treeFileNames :: [FilePath]
treeFileNames = (<> "-leaf-tree") . show . (bit :: Int -> Word) <$> [2..12]
