------------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.GeneratedTests.Nexus
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module TestSuite.GeneratedTests.Nexus
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.Nexus
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal


-- |
-- Test-suite generated to assert that the Nexus file parser successfully parses
-- the files that it is expected to accept and fails to parse the files that it
-- is expected to reject.
testSuite :: IO TestTree
testSuite = testGroup "nexusStreamParser" <$> sequence [validNexusFiles, invalidNexusFiles]


validNexusFiles :: IO TestTree
validNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "nexus/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess nexusStreamParser content


invalidNexusFiles :: IO TestTree
invalidNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "nexus/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure nexusStreamParser content
