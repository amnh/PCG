-- {-# LANGUAGE FlexibleContexts    #-}
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


measureParserSpace
  :: ( NFData a
     , Stream s
     )
  => String
  -> FilePath
  -> (FilePath -> IO s)
  -> ParsecT Void s Identity a
  -> Weigh ()
measureParserSpace prefix filePath streamReader streamParser =
    io (prefix </> filePath) (\fp -> forceParser streamParser fp <$> streamReader fp) filePath


measureParserTime
  :: ( NFData a
     , NFData s
     , Stream s
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
     , Stream s
     )
  => ParsecT Void s Identity a
  -> FilePath
  -> s
  -> a
forceParser parser filePath = either (error . errorBundlePretty) force . parse parser filePath
