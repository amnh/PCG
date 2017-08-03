{-# LANGUAGE DoAndIfThenElse #-}
module TestSuite.GeneratedTests.Internal where

import Control.Arrow    ((&&&))
import Data.Map         (Map,fromList)
import System.Directory

-- | Gets all the given files and thier contents in the specified directory
getFileContentsInDirectory :: FilePath -> IO (Map FilePath String)
getFileContentsInDirectory path = do
    exists <- doesDirectoryExist path
    if not exists
    then pure mempty
    else (do
      files  <- filter isFile <$> getDirectoryContents path
      sequence . fromList $ (id &&& readFile) . withPath <$> files)
  where
    isFile = not . all (=='.')
    withPath file = if last path /= '/'
                    then path ++ "/" ++ file
                    else path ++ file
