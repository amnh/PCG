module Benchmark.FASTC.Files
  ( fastcFilePath
  , fastcSequenceFiles
  ) where

import Data.Foldable
import System.FilePath.Posix


fastcSequenceFiles :: [FilePath]
fastcSequenceFiles = do
    taxaCount      <- taxaCounts
    sequenceLength <- sequenceLengths
    symbolSize     <- symbolSizes
    let fileName = fold [ show taxaCount, "x", show sequenceLength, "+", show symbolSize, ".fastc" ]
    pure $ fastcFilePath </> fileName


fastcFilePath :: FilePath
fastcFilePath =  "bench" </> "data-sets" </>"fastc"


symbolSizes :: [Word]
symbolSizes = (2 *) . (2^) <$> [0 .. 3 :: Word]


taxaCounts :: [Word]
taxaCounts = (4 *) . (4^) <$> [0 .. 3 :: Word]


sequenceLengths :: [Word]
sequenceLengths = (16 *) . (4^) <$> [0 .. 3 :: Word]

