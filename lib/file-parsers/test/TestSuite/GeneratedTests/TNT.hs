------------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.GeneratedTests.TNT
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

module TestSuite.GeneratedTests.TNT
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.TNT
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal


-- |
-- Test-suite generated to assert that the TNT file parser successfully parses
-- the files that it is expected to accept and fails to parse the files that it
-- is expected to reject.
testSuite :: IO TestTree
testSuite = testGroup "tntStreamParser" <$> sequence [validTNTFiles, invalidTNTFiles]


validTNTFiles :: IO TestTree
validTNTFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "tnt/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess tntStreamParser content


invalidTNTFiles :: IO TestTree
invalidTNTFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "tnt/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure tntStreamParser content
