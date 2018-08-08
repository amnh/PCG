{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.Nexus
  ( testSuite
  ) where

import           Data.Map                          (toList)
import           File.Format.Nexus
import           Test.Custom.Parse
import           Test.Tasty                        (TestTree, testGroup)
import           Test.Tasty.HUnit
import           TestSuite.GeneratedTests.Internal

testSuite :: IO TestTree
testSuite = testGroup "nexusStreamParser" <$> sequence [validNexusFiles, invalidNexusFiles]

validNexusFiles :: IO TestTree
validNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/nexus/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess nexusStreamParser content

invalidNexusFiles :: IO TestTree
invalidNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/nexus/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure nexusStreamParser content
