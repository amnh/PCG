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
import Turtle                (shell, cd, pwd, rm, decodeString, encodeString)


testSuite :: IO TestTree
testSuite = do
  pcgFiles <- getPCGFiles goldenDir
  let extensions = ["data", "dot", "xml"]
  let testInputs = [(pcg, ext) | pcg <- pcgFiles, ext <- extensions]
  flip shell mempty
    $ pack
      $ mconcat
        ["[ -e ", testLog, " ]", " && ", "rm ", testLog] -- deletes the previous test.log
  tests <- traverse goldenTest testInputs
  pure $
    testGroup "Golden Test Suite:"
    tests
  where
    goldenDir :: FilePath
    goldenDir = "./datasets/golden-tests"

-- |
-- Runs a pcg file [file-name].pcg producing [file-name].extension for a
-- given extension which is then compared to [file-name]_extension.golden. If the
-- golden file does not exist the test will generate it.
goldenTest :: (FilePath, String) -> IO TestTree
goldenTest (filePath, extension) = do
  let testName = filePath
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
generateOutput fp
  = do
  baseDir <- pwd
  let testFP = pack $ (encodeString baseDir) </> testLog
  cd $ decodeString fileDir
  flip shell mempty $
    mconcat
      ["stack exec pcg"
      , "< "
      , pack fileName
      , " >>"
      , testFP
      ]
  cd baseDir
  where
    (fileDir, fileName) = splitFileName fp


-- |
-- Recursively gets all .pcg files in a directory.
getPCGFiles :: FilePath -> IO [FilePath]
getPCGFiles fp = do
  subDirs <- getSubDirs fp
  concat <$> traverse getPCGFilesInDir subDirs
    where
      getPCGFilesInDir :: FilePath -> IO [FilePath]
      getPCGFilesInDir =
        let filterPCG = filter $ (== ".pcg") . takeExtension
          in  (filterPCG <$>) . listDirectoryWithFilePath

      getSubDirs :: FilePath -> IO [FilePath]
      getSubDirs =
          (filterM doesDirectoryExist =<<)
        . listDirectoryWithFilePath

-- |
-- Takes a directory filepath and returns a list of all files within
-- the directory with full filepaths.
listDirectoryWithFilePath :: FilePath -> IO [FilePath]
listDirectoryWithFilePath fp
  = (fmap (fp </>) <$>)
  . listDirectory
  $ fp


-- |
-- Name of test log file.
testLog :: FilePath
testLog = "test" <.> "log"
