{-# LANGUAGE FlexibleContexts #-}

module TestSuite.GeneratedTests.Fastc
  ( testSuite
  ) where

--import Data.Either.Combinators
import Data.Map                          (toList)
import File.Format.Fastc
import Test.Tasty                        (TestTree,testGroup)
import Test.Custom.Parse
import Test.Tasty.HUnit
import TestSuite.GeneratedTests.Internal
import Text.Megaparsec                   (parse)

testSuite :: IO TestTree
testSuite = testGroup "fastcStreamParser" <$> sequence [validFastaFiles{-, invalidFastaFiles-}]

validFastaFiles :: IO TestTree
validFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/fastc/valid"
    validateFileContents   = testGroup "Valid files" . fmap success . toList
    success (path,content) = testCase (show path) $ parseSuccess fastcStreamParser content 

{-
invalidFastaFiles :: IO TestTree
invalidFastaFiles = validateFileContents <$> validContents
  where
    validContents          = getFileContentsInDirectory "test/data-sets/fasta/invalid"
    validateFileContents   = testGroup "Invalid files" . fmap failure . toList
    failure (path,content) = testCase (show path) . assert $ isLeft result
      where
        result = parse fastaStreamParser path content
-}
