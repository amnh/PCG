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

module Main (main) where

import           Criterion.Main
import qualified Data.MutualExclusionSet.Bench  as MutualExclusionSet
import qualified Numeric.Extended.Natural.Bench as ExtendedNatural


-- |
-- Entry point for benchmarks of the utility data types.
main :: IO ()
main = defaultMain
    [ MutualExclusionSet.benchmarks
    , ExtendedNatural.benchmarks
    ]
