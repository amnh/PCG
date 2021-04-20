------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.VER.Time
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

module Benchmark.VER.Time
  ( benchTime
  ) where

import           Benchmark.Internal         (measureParserTime)
import           Benchmark.VER.Files
import           Control.DeepSeq            (NFData)
import           Criterion.Main
import           Data.CaseInsensitive       (FoldCase)
import           Data.Foldable
import qualified Data.Text.Lazy.IO          as TL
import           File.Format.VertexEdgeRoot
import           Text.Megaparsec


-- |
-- Perform the run time benchmarking of the VER file parser.
benchTime :: [Benchmark]
benchTime = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> verFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( FoldCase (Tokens s)
     , NFData s
     , Token s ~ Char
     , TraversableStream s
     , VisualStream s
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Benchmark
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader verStreamParser
