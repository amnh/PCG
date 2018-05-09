module Main
  ( main
  ) where

import Test.Tasty
import TestSuite.GeneratedTests

main :: IO ()
main = testSuite >>= defaultMain


