{-# LANGUAGE OverloadedStrings #-}
module TestSuite.GoldenTests
  ( testSuite
  ) where

import Control.Monad         (filterM, mapM, void)
import Data.Text             (pack)
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.Golden
import Turtle                (shell)


testSuite :: IO TestTree
testSuite = do
  pcgFiles <- getPCGFiles goldenDir
  let extensions = ["data", "dot", "xml"]
  let testInputs = [(pcg, ext) | pcg <- pcgFiles, ext <- extensions]
  tests <- traverse goldenTest testInputs
  pure $
    testGroup "Golden Test Suite:"
    tests
  where
    --TODO (CM): put correct dir here"
    goldenDir :: FilePath
    goldenDir = "./datasets/golden-tests"

-- |
-- Runs a pcg file [file-name].pcg producing [file-name].extension for a
-- given extension which is then compared to [file-name]_extension.golden. If the
-- golden file does not exist the test will generate it.
goldenTest :: (FilePath, String) -> IO TestTree
goldenTest (filePath, extension) = do
  let testName = getFileName filePath
  let outputFilePath = filePath -<.> extension
  let goldenFilePath = makeGolden filePath
  pure $
    goldenVsFile
      testName
      goldenFilePath
      outputFilePath
      (generateOutput filePath)
  where
    getFileName :: FilePath -> FilePath
    getFileName =  dropExtension . takeFileName

    makeGolden :: FilePath -> FilePath
    makeGolden = (<.> "golden") . (<> "_" <> extension) . dropExtension

-- |
-- Runs pcg on the file passed in.
generateOutput :: FilePath -> IO ()
generateOutput
  = void
  . flip shell mempty
  . ("stack exec pcg -- --output test.log < " <>)
  . pack

-- |
-- Recursively gets all .pcg files in a directory.
getPCGFiles :: FilePath -> IO [FilePath]
getPCGFiles fp = do
  subDirs <- getSubDirs fp
  pcg     <- concat <$> traverse getPCGFiles subDirs
  pure pcg
    where
      getPCGFiles :: FilePath -> IO [FilePath]
      getPCGFiles =
        let filterPCG = filter $ (== ".pcg") . takeExtension
          in  (filterPCG <$>) . listDirectoryWithFilePath

      getSubDirs :: FilePath -> IO [FilePath]
      getSubDirs =
          (filterM doesDirectoryExist =<<)
        . listDirectoryWithFilePath

listDirectoryWithFilePath :: FilePath -> IO [FilePath]
listDirectoryWithFilePath fp = do
  (fmap (fp </>) <$>) . listDirectory $ fp
