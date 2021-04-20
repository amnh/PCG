------------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.GeneratedTests.Fastc
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

module TestSuite.GeneratedTests.Fastc
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.Fastc
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal


-- |
-- Test-suite generated to asster that the FASTC file parser successfully parses
-- the files that it is expected to accept and fails to parse the files that it
-- is expected to reject.
testSuite :: IO TestTree
--testSuite = testGroup "fastcStreamParser" <$> sequence [validFastaFiles, invalidFastaFiles]
testSuite = testGroup "fastcStreamParser" . pure <$> validFastaFiles


validFastaFiles :: IO TestTree
validFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "fastc/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastcStreamParser content
