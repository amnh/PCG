------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.Newick.Time
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Benchmark.Newick.Time
  ( benchTime
  ) where

import           Benchmark.Internal     (measureParserTime)
import           Benchmark.Newick.Files
import           Control.DeepSeq        (NFData)
import           Criterion.Main
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO      as TL
import           File.Format.Newick
import           Text.Megaparsec


-- |
-- Perform the run time benchmarking of the eNewick file parser.
benchTime :: [Benchmark]
benchTime = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> newickInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( NFData s
     , Token s ~ Char
     , TraversableStream s
     , VisualStream s
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Benchmark
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader newickStreamParser
