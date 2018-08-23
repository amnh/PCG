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
  dir      <- goldenDir
  pcgFiles <- getPCGFiles dir
  let extensions = ["dot"] -- TODO (CM): Add extensions for output to be tested.
  let testInputs = [(pcg, ext) | pcg <- pcgFiles, ext <- extensions]
  tests <- mapM goldenTest testInputs
  pure $
    testGroup "Golden Test Suite:"
    tests
  where
    --TODO (CM): put correct dir here"
    goldenDir :: IO FilePath
    goldenDir = makeAbsolute "./datasets/golden-tests"

-- |
-- Runs a pcg file [file-name].pcg producing [file-name].extension for a
-- given extension which is then compared to [file-name].golden. If the
-- golden file does not exist the test will generate it.
goldenTest :: (FilePath, String) -> IO TestTree
goldenTest (filePath, extension) = do
  let testName = getFileName filePath
  let outputFilePath = filePath -<.> extension
  let goldenFilePath = filePath -<.> "golden"
  pure $
    goldenVsFile
      testName
      goldenFilePath
      outputFilePath
      (generateOutput filePath)
  where
    getFileName :: FilePath -> FilePath
    getFileName =  dropExtension . takeFileName

-- |
-- Runs pcg on the file passed in.
generateOutput :: FilePath -> IO ()
generateOutput
  = void
  . flip shell mempty
  . ("stack exec pcg < " <>)
  . pack

-- |
-- Recursively gets all .pcg files in a directory.
getPCGFiles :: FilePath -> IO [FilePath]
getPCGFiles fp = do
  subDirs <- getSubDirs fp
  pcg     <- concat <$> mapM getPCGFiles subDirs
  pure pcg
    where
      getPCGFiles :: FilePath -> IO [FilePath]
      getPCGFiles fp =
        let filterPCG = filter $ (== ".pcg") . takeExtension
          in (fmap (fp </>) <$>) . (filterPCG <$>) . listDirectory $ fp

      getSubDirs :: FilePath -> IO [FilePath]
      getSubDirs fp =
          (filterM doesDirectoryExist =<<)
        . fmap ((fp </>) <$>)
        . listDirectory
        $ fp
