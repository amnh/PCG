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

module Main ( main ) where

import qualified Control.Parallel.Test           as Parallel
import qualified Data.BitMatrix.Test             as BitMatrix
import qualified Data.List.Test                  as DataList
import qualified Data.MutualExclusionSet.Test    as MutualExclusionSet
import qualified Numeric.Cost.Test               as Cost
import qualified Numeric.Extended.Natural.Test   as ExtendedNatural
import qualified Numeric.Extended.Real.Test      as ExtendedReal
import qualified Numeric.NonNegativeAverage.Test as NonNegativeAverage
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun    (rerunningTests)


-- |
-- Entry point for the test-suite of the utility library.
main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests defaultIngredients ]
  testSuite


testSuite :: TestTree
testSuite = testGroup "Library Test Suite"
    [ Parallel.testSuite
    , BitMatrix.testSuite
    , Cost.testSuite
    , ExtendedNatural.testSuite
    , ExtendedReal.testSuite
    , NonNegativeAverage.testSuite
    , MutualExclusionSet.testSuite
    , DataList.testSuite
    ]
