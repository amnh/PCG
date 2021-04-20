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

import Test.Tasty
import TestSuite.GeneratedTests


-- |
-- Entry point for the "file test-suite" which asserts that the /all/ file
-- parsers successfully parse the files that they are expected to accept and
-- fail to parse the files that they are expected to reject.
main :: IO ()
main = testSuite >>= defaultMain


