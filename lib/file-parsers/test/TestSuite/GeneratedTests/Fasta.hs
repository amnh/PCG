------------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.GeneratedTests.Fasta
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

module TestSuite.GeneratedTests.Fasta
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.Fasta
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal


-- |
-- Test-suite generated to asster that the FASTA file parser successfully parses
-- the files that it is expected to accept and fails to parse the files that it
-- is expected to reject.
testSuite :: IO TestTree
testSuite = testGroup "fastaStreamParser" <$> sequence [validFastaFiles, invalidFastaFiles]


validFastaFiles :: IO TestTree
validFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "fasta/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastaStreamParser content


invalidFastaFiles :: IO TestTree
invalidFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "fasta/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure fastaStreamParser content
