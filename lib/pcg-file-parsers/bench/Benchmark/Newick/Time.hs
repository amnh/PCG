{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.Newick.Time
  ( benchTime
  , newickFilePath
  , newickInlineSequenceFiles
  ) where

import           Benchmark.Internal    (measureParserTime)
import           Control.DeepSeq       (NFData)
import           Criterion.Main
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Newick
import           System.FilePath.Posix
import           Text.Megaparsec


benchTime :: [Benchmark]
benchTime = mconcat
    [ parserBenchmark ("lazy-text", TL.readFile) <$> newickInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( NFData s
     , Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Benchmark
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader newickStreamParser


newickInlineSequenceFiles :: [FilePath]
newickInlineSequenceFiles = do
    taxaCount      <- taxaCounts
    let fileName = fold [ show taxaCount, ".tree" ]
    pure $ newickFilePath </> fileName


newickFilePath :: FilePath
newickFilePath =  "bench" </> "data-sets" </>"newick"


taxaCounts :: [Word]
taxaCounts = (8 *) . (2^) <$> [0 .. 5 :: Word]
