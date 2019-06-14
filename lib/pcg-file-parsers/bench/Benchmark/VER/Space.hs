{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.VER.Space
  ( benchSpace
  ) where

import           Benchmark.Internal               (measureParserSpace)
import           Benchmark.VER.Files
import           Data.CaseInsensitive   (FoldCase)
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO                as TL
import           File.Format.VertexEdgeRoot
import           Text.Megaparsec
import           Weigh


benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> verFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastaInlineSequenceFiles
    ]


parserBenchmark
  :: ( FoldCase (Tokens s)
     , Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader verStreamParser
