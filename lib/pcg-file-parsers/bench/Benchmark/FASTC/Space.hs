{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.FASTC.Space
  ( benchSpace
  ) where

import           Benchmark.FASTC.Files
import           Benchmark.Internal    (measureParserSpace)
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Fastc
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
