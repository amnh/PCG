------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.FASTC.Space
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


-- |
-- Perform the space allocation benchmarking of the FASTC file parser.
benchSpace :: [Weigh ()]
benchSpace = fold
    [ parserBenchmark ("lazy-text", TL.readFile) <$> fastcSequenceFiles
--    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
    ]


parserBenchmark
  :: ( Token s ~ Char
     , TraversableStream s
     , VisualStream s
     )
  => (String, FilePath -> IO s)
  -> FilePath
  -> Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader fastcStreamParser
