------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

import Benchmark.StringAlignment


-- |
-- Entry point for the /full/ string alignment benchmarks.
main :: IO ()
main = benchStringAlignment $ const True
