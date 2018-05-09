{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.Fasta 
  ( testSuite
  ) where

import Data.Map                          (toList)
import File.Format.Fasta
import Test.Custom.Parse
import Test.Tasty                        (TestTree,testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal

testSuite :: IO TestTree
testSuite = testGroup "fastaStreamParser" <$> sequence [validFastaFiles, invalidFastaFiles]

validFastaFiles :: IO TestTree
validFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/fasta/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastaStreamParser content

invalidFastaFiles :: IO TestTree
invalidFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/fasta/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) $ parseFailure fastaStreamParser content
