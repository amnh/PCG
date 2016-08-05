module Main where

import Tst

main :: IO ()
main = do
  a <- makeA 123
  runA a 456
