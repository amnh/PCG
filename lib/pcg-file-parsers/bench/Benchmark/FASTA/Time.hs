{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.FASTA.Time
  ( benchTime
  , fastaFilePath
  , fastaInlineSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserTime)
import           Control.DeepSeq       (NFData)
import           Criterion.Main
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fasta
import           System.FilePath.Posix
import           Text.Megaparsec


benchTime :: [Benchmark]
benchTime = mconcat
    [ parserBenchmark ("lazy-text", TL.readFile) <$> fastaInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( NFData s
     , Stream s
     , Token s ~ Char
     , Monoid (Tokens s)
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Benchmark
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader fastaStreamParser


fastaInlineSequenceFiles :: [FilePath]
fastaInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    sequenceLength <- sequenceLengths
    let fileName = fold [ show taxaCount, "x", show sequenceLength, ".fasta" ]
    pure $ fastaFilePath </> fileName


fastaFilePath :: FilePath
fastaFilePath =  "bench" </> "data-sets" </>"fasta"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 5 :: Word]


sequenceLengths :: [Word]
sequenceLengths = (8 *) . (2^) <$> [0 .. 9 :: Word]
