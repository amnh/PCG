-- |
-- Collects file paths in a directory which are to be converted into unit tests.
{-# LANGUAGE DoAndIfThenElse #-}

module TestSuite.GeneratedTests.Internal where

import           Control.Arrow    ((&&&))
import           Data.Map         (Map, fromList)
import           Data.Text        (Text)
import qualified Data.Text.IO     as T (readFile)
import           System.Directory


-- |
-- The directory containing all the test files.
pathPrefix :: String
pathPrefix = "lib/file-parsers/test/data-sets/"


-- |
-- Gets all the given files and their contents in the specified directory
getFileContentsInDirectory :: FilePath -> IO (Map FilePath Text)
getFileContentsInDirectory path = do
    let sep | head path /= '/' && last pathPrefix /= '/' = "/"
            | otherwise = ""
    let fullPath = pathPrefix <> sep <> path
    exists <- doesDirectoryExist fullPath
    if not exists
    then pure mempty
    else do
      files  <- filter isFile <$> getDirectoryContents fullPath
      sequence . fromList $ (id &&& T.readFile) . withPath fullPath <$> files
  where
    isFile = not . all (=='.')
    withPath p file
      | last p /= '/' = p <> "/" <> file
      | otherwise     = p        <> file
