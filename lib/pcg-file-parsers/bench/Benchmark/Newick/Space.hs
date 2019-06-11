{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.Newick.Space
  ( benchSpace
  ) where

import           Benchmark.Internal    (measureParserSpace)
import           Benchmark.Newick.Files
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO     as TL
import           File.Format.Newick
import           Text.Megaparsec
import           Weigh


benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> newickInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> newickInlineSequenceFiles
    ]


parserBenchmark
  :: ( Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader newickStreamParser
