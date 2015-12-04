{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.Nexus
  ( testSuite
  ) where

import Data.Either.Combinators
import Data.Map                          (toList)
import File.Format.Nexus
import Test.Tasty                        (TestTree,testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal
import Text.Megaparsec                   (parse)

testSuite :: IO TestTree
testSuite = testGroup "nexusStreamParser" <$> sequence [validNexusFiles, invalidNexusFiles]

validNexusFiles :: IO TestTree
validNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/nexus/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) . assert $ isRight result
--                           $ case result of
--                               Left x  -> assertFailure $ show x
--                               Right _ -> assert True
      where
        result = parse nexusStreamParser path content

invalidNexusFiles :: IO TestTree
invalidNexusFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/nexus/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) . assert $ isLeft result
      where
        result = parse nexusStreamParser path content
