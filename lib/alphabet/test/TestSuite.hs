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

module Main where

import Data.Alphabet.Test
import Test.Tasty
import Test.Tasty.Ingredients.Rerun (rerunningTests)


-- |
-- The entry point for the test-suite of the 'Data.Alphabet.Alphabet' data type.
main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite
