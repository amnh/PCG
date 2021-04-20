------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.Newick.Space
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

module Benchmark.Newick.Space
  ( benchSpace
  ) where

import           Benchmark.Internal     (measureParserSpace)
import           Benchmark.Newick.Files
import           Data.Foldable
--import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO      as TL
import           File.Format.Newick
import           Text.Megaparsec
import           Weigh


-- |
-- Perform the space allocation benchmarking of the eNewick file parser.
benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> newickInlineSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> newickInlineSequenceFiles
    ]


parserBenchmark
  :: ( Token s ~ Char
     , TraversableStream s
     , VisualStream s
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader newickStreamParser
