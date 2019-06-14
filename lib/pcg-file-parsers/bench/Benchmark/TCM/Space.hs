{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.TCM.Space
  ( benchSpace
  ) where

import           Benchmark.Internal               (measureParserSpace)
import           Benchmark.TCM.Files
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO                as TL
import           File.Format.TransitionCostMatrix
import           Text.Megaparsec
import           Weigh


benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> tcmFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastaInlineSequenceFiles
    ]


parserBenchmark
  :: ( Stream s
     , Token s ~ Char
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader tcmStreamParser
