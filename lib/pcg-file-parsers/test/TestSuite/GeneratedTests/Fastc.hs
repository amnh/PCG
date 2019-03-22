{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.Fastc
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.Fastc
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal

testSuite :: IO TestTree
--testSuite = testGroup "fastcStreamParser" <$> sequence [validFastaFiles, invalidFastaFiles]
testSuite = testGroup "fastcStreamParser" . pure <$> validFastaFiles

validFastaFiles :: IO TestTree
validFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/fastc/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastcStreamParser content
