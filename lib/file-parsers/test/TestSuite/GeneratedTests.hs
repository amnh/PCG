------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Cost.Test
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module TestSuite.GeneratedTests
  ( testSuite
  ) where

import           Test.Tasty                     (TestTree, testGroup)
import qualified TestSuite.GeneratedTests.Fasta as Fasta (testSuite)
import qualified TestSuite.GeneratedTests.Fastc as Fastc (testSuite)
--import qualified TestSuite.GeneratedTests.Nexus as Nexus (testSuite)
import qualified TestSuite.GeneratedTests.TNT   as TNT (testSuite)


-- |
-- Test-suite which asserts that the /all/ file parsers successfully parse the
-- files that they are expected to accept and fail to parse the files that they
-- are expected to reject.
testSuite :: IO TestTree
testSuite = testGroup "Dynamically generated tests"
           <$> sequence
             [ Fasta.testSuite
             , Fastc.testSuite
--             , Nexus.testSuite
             , TNT.testSuite
             ]
