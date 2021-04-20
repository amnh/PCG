------------------------------------------------------------------------------
-- |
-- Module      :  Benchmark.Internal
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Benchmark.Internal
  ( measureParserSpace
  , measureParserTime
  ) where

import Control.DeepSeq
import Criterion.Main
import Data.Functor.Identity
import Data.Void
import System.FilePath.Posix
import Text.Megaparsec
import Weigh


-- |
-- Abstract utility function for measuring the allocated space of the supplied
-- file parser on the specified input file.
measureParserSpace
  :: ( NFData a
     , TraversableStream s
     , VisualStream s
     )
  => String
  -> FilePath
  -> (FilePath -> IO s)
  -> ParsecT Void s Identity a
  -> Weigh ()
measureParserSpace prefix filePath streamReader streamParser =
    io (prefix </> filePath) (\fp -> forceParser streamParser fp <$> streamReader fp) filePath


-- |
-- Abstract utility function for measuring the run time of the supplied
-- file parser on the specified input file.
measureParserTime
  :: ( NFData a
     , NFData s
     , TraversableStream s
     , VisualStream s
     )
  => String
  -> FilePath
  -> (FilePath -> IO s)
  -> ParsecT Void s Identity a
  -> Benchmark
measureParserTime prefix filePath streamReader streamParser =
    env (streamReader filePath) (bench (prefix </> filePath) . nf (forceParser streamParser filePath))


forceParser
  :: ( NFData a
     , TraversableStream s
     , VisualStream s
     )
  => ParsecT Void s Identity a
  -> FilePath
  -> s
  -> a
forceParser parser filePath = either (error . errorBundlePretty) force . parse parser filePath
