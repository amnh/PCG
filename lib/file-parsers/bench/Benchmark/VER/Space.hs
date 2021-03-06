------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.VER.Space
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

module Benchmark.VER.Space
  ( benchSpace
  ) where

import           Benchmark.Internal         (measureParserSpace)
import           Benchmark.VER.Files
import           Data.CaseInsensitive       (FoldCase)
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO          as TL
import           File.Format.VertexEdgeRoot
import           Text.Megaparsec
import           Weigh


-- |
-- Perform the space allocation benchmarking of the VER file parser.
benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> verFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastaInlineSequenceFiles
    ]


parserBenchmark
  :: ( FoldCase (Tokens s)
     , Token s ~ Char
     , TraversableStream s
     , VisualStream s
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader verStreamParser
