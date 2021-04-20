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


import qualified File.Format.Fasta.Test                as Fasta
import qualified File.Format.Fastc.Test                as Fastc
import qualified File.Format.Newick.Test               as Newick
--import qualified File.Format.Nexus.Test                as Nexus
import qualified File.Format.TNT.Test                  as TNT
import qualified File.Format.TransitionCostMatrix.Test as TCM
import qualified File.Format.VertexEdgeRoot.Test       as VER
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun          (rerunningTests)
import qualified Text.Megaparsec.Custom.Test           as Megaparsec


-- |
-- Entry point for file parser test-suite.
main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


-- |
-- Test-suite including specific unit and property-based tests for /all/ file parsers.
testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [ Megaparsec.testSuite
    , Fasta.testSuite
    , Fastc.testSuite
    , Newick.testSuite
--    , Nexus.testSuite
    , TNT.testSuite
    , TCM.testSuite
    , VER.testSuite
    ]
