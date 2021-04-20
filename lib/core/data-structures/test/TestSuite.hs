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
  , testSuite
  ) where

import qualified Bio.Character.Encodable.Dynamic.Test as DynamicChar
import qualified Bio.Character.Encodable.Static.Test  as StaticChar
import qualified Bio.Graph.ReferenceDAG.Test          as ReferenceDAG
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun         (rerunningTests)


-- |
-- Entry point for the test-suite of the "data-structures" library.
main :: IO ()
main = defaultMainWithIngredients
    [ rerunningTests defaultIngredients ]
    testSuite


-- |
-- Collection of test-suites for the "data-structures" library.
testSuite :: TestTree
testSuite = testGroup "PCG data-structure library test suite"
    [ DynamicChar.testSuite
    , StaticChar.testSuite
    , ReferenceDAG.testSuite
    ]
