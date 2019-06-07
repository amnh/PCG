{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.FASTC.Space
  ( benchSpace
  , fastcFilePath
  , fastcSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserSpace)
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fastc
import           System.FilePath.Posix
import           Text.Megaparsec
import           Weigh


benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> fastcSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader fastcStreamParser


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

