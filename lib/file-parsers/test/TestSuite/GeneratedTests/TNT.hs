{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.TNT
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.TNT
import Test.Custom.Parse
import Test.Tasty                        (TestTree, testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal

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
