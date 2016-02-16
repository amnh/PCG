{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.TNT
  ( testSuite
  ) where

import Data.Either.Combinators
import Data.Map                          (toList)
import File.Format.TNT
import Test.Tasty                        (TestTree,testGroup)
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal
import Text.Megaparsec                   (parse)

testSuite :: IO TestTree
testSuite = testGroup "tntStreamParser" <$> sequence [validTNTFiles, invalidTNTFiles]

validTNTFiles :: IO TestTree
validTNTFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/tnt/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) . assert $ isRight result
--                           $ case result of
--                               Left x  -> assertFailure $ show x
--                               Right _ -> assert True
      where
        result = parse tntStreamParser path content

invalidTNTFiles :: IO TestTree
invalidTNTFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/tnt/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) . assert $ isLeft result
      where
        result = parse tntStreamParser path content
