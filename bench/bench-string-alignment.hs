module Main
  ( main
  ) where

import Benchmark.StringAlignment


main :: IO ()
main = benchStringAlignment $ const True
