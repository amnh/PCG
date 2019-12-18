module Benchmark.Newick.Files
  ( newickFilePath
  , newickInlineSequenceFiles
  ) where

import Data.Foldable
import System.FilePath.Posix


newickInlineSequenceFiles :: [FilePath]
newickInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    let fileName = fold [ show taxaCount, ".tree" ]
    pure $ newickFilePath </> fileName


newickFilePath :: FilePath
newickFilePath =  "bench" </> "data-sets" </>"newick"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 9 :: Word]
