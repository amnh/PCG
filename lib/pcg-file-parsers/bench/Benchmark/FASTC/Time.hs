{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.FASTC.Time
  ( benchTime
  ) where

import           Benchmark.FASTC.Files
import           Benchmark.Internal    (measureParserTime)
import           Control.DeepSeq
import           Criterion.Main
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fastc
import           Text.Megaparsec


benchTime :: [Benchmark]
benchTime = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> fastcSequenceFiles
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
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader fastcStreamParser
