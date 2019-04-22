-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource.IO
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes several useful disk utility related functionality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.FileSource.IO
  ( -- * Input
    readFile
  , readFiles
  , readStdIn
    -- * Output
  , appendFile
  , writeFile
  , writeFileWithMove
    -- * Binary data I/O
  , deserializeBinary
  , serializeBinary
    -- * Compact region I/O
  , deserializeCompact
  , serializeCompact
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Validation
import           Data.Binary                       (Binary, decodeFileOrFail, encodeFile)
import           Data.Char                         (isNumber)
import           Data.Compact                      (Compact)
import           Data.Compact.Serialize            (unsafeReadCompact, writeCompact)
import           Data.FileSource
import           Data.FileSource.InputStreamError
import           Data.FileSource.OutputStreamError
import           Data.Foldable
import           Data.List                         (isPrefixOf)
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.MonoTraversable
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text.IO                      as T (appendFile, getContents, readFile, writeFile)
import           Data.Typeable                     (Typeable)
import           Data.Validation
import           Prelude                           hiding (appendFile, getContents, readFile, writeFile)
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix             (takeDirectory, takeExtension)
import           System.IO                         hiding (appendFile, putStrLn, readFile, writeFile)
import           System.IO.Error


-- |
-- Read textual the contents of a file.
--
-- If the 'FileSource' exists verbatim, the contents will be read.
--
-- If the 'FileSource' does not exist verbatim, the 'FileSource' will be
-- interpreted as a "file-glob" pattern. If there exists a single, unabiguous
-- file matching the "file-glob" pattern, the contents will be read. If there
-- exist multiple files which match the "file-glob" pattern, an "ambiguous file"
-- failure state will be returned.
readFile :: FileSource -> ValidationT InputStreamError IO Text
readFile filePath =
    readFilesAndLocate
      readFileContent
      (invalid . makeAmbiguousFiles filePath)
      filePath


-- |
-- Read the textual contents of one or more files matching a "file globbing" pattern.
--
-- If the 'FileSource' exists verbatim, the contents will be read.
--
-- If the 'FileSource' does not exist verbatim, the 'FileSource' will be
-- interpreted as a "file-glob" pattern. The contents of each each file matching
-- the glob pattern will be read, and the name of the matching file will be
-- tagged to the corresponding file contents.
readFiles :: FileSource -> ValidationT InputStreamError IO (NonEmpty (FileSource, Text))
readFiles =
    readFilesAndLocate
      (fmap pure . readContentsAndTag)
      (traverse readContentsAndTag)
  where
    readContentsAndTag :: FileSource -> ValidationT InputStreamError IO (FileSource, Text)
    readContentsAndTag path = (\z -> (path, z)) <$> readFileContent path


-- |
-- Read textual the contents of a file.
--
-- Returns a 'Failure' if the STDIN stream is empty.
readStdIn :: ValidationT InputStreamError IO Text
readStdIn = do
    nonEmptyStream <- liftIO $ hReady stdin
    if   nonEmptyStream
    then liftIO T.getContents
    else invalid $ makeEmptyFileStream "STDIN"


-- |
-- Write textual stream to the /end/ of the file.
appendFile :: FileSource -> Text -> ValidationT OutputStreamError IO ()
appendFile filePath txt = ValidationT $ catch
      (Success <$> T.appendFile (otoList filePath) txt)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Write textual stream to the to the file, overwriting any exisiting file contents.
writeFile :: FileSource -> Text -> ValidationT OutputStreamError IO ()
writeFile filePath txt = ValidationT $ catch
      (Success <$> T.writeFile (otoList filePath) txt)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Write textual stream to the to the file.
--
-- If the specified file already exisits, rename the existing file, so that the
-- specified file can be written to without overwriting exisiting data.
--
-- The exisiting file is renamed, adding a numeric suffix to the end. The
-- function will try to rename the existing file path by adding the suffix ".0",
-- however if that filepath also exists, it will add ".1", ".2", ".3", ",.4", etc.
-- The suffix added will be one greater than the highest existing numeric suffix.
writeFileWithMove :: FileSource -> Text -> ValidationT OutputStreamError IO ()
writeFileWithMove filePath txt = liftIO (safelyMoveFile filePath) *> writeFile filePath txt


-- |
-- Deserialize binary encodable content from the specified file path.
--
-- Operational inverse of 'serializeBinary'.
deserializeBinary :: Binary a => FileSource -> ValidationT InputStreamError IO a
deserializeBinary filePath =
    readFilesAndLocate
      deserialize
      (invalid . makeAmbiguousFiles filePath)
      filePath
  where
    deserialize fp = do
        res <- ValidationT $ catch
                   (Success <$> decodeFileOrFail (otoList fp))
                   (runValidationT . inputErrorHandling fp)
        case res of
          Left  (_, err) -> invalid . makeFileDeserializeErrorInBinaryEncoding fp $ fromString err
          Right val      -> pure val


-- |
-- Serialize binary encodable content to the specified file path.
--
-- Operational inverse of 'deserializeBinary'.
serializeBinary :: Binary a => FileSource -> a -> ValidationT OutputStreamError IO ()
serializeBinary filePath val =
    ValidationT $ catch
      (Success <$> encodeFile (otoList filePath) val)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Deserialize a compact region from the specified file path.
--
-- Operational inverse of 'serializeCompact'.
deserializeCompact :: Typeable a => FileSource -> ValidationT InputStreamError IO (Compact a)
deserializeCompact filePath =
    readFilesAndLocate
      deserialize
      (invalid . makeAmbiguousFiles filePath)
      filePath
  where
    deserialize fp = do
        res <- ValidationT $ catch
                   (Success <$> unsafeReadCompact (otoList fp))
                   (runValidationT . inputErrorHandling fp)
        case res of
          Left  err -> invalid . makeFileDeserializeErrorInCompactRegion fp $ fromString err
          Right val -> pure val


-- |
-- Serialize a compact region's contents to the specified file path.
--
-- Operational inverse of 'deserializeCompact'.
serializeCompact :: Typeable a => FileSource -> Compact a -> ValidationT OutputStreamError IO ()
serializeCompact filePath val =
    ValidationT $ catch
      (Success <$> writeCompact (otoList filePath) val)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Read the textual contents of one or more files matching a "file globbing" pattern.
readFilesAndLocate
  :: (FileSource -> ValidationT InputStreamError IO a)          -- ^ What to do in the unambiguous case
  -> (NonEmpty FileSource -> ValidationT InputStreamError IO a) -- ^ What to do in the ambiguous case
  -> FileSource -- ^ Path description
  -> ValidationT InputStreamError IO a -- ^ Result
readFilesAndLocate f g filePath = do
    -- Check if the file exists exactly as specified
    exists <- liftIO $ doesFileExist (otoList filePath)
    if   exists
    -- If it exists exactly as specified, read it in
    then f filePath
    else do
        -- If the file does not exists exactly as specified
        -- try to match other files to the given path
        -- by interpreting the path as a 'glob'
        matches <- fmap (fmap fromString) . liftIO . glob $ otoList filePath
        case matches of
          []   -> invalid $ makeFileNotFound filePath
          [x]  -> f x
          x:xs -> g $ x:|xs


-- |
-- Shared utility between 'readFile' & 'readFiles'.
--
-- Checks if the file permissions allows the file contents to be read.
readFileContent :: FileSource -> ValidationT InputStreamError IO Text
readFileContent filePath =
    let path = force $ otoList filePath
    in  do
      canRead <- liftIO $ readable <$> getPermissions path
      if   not canRead
      then invalid $ makeFileNoReadPermissions filePath
      else do
        txt <- ValidationT $ catch
                 (Success <$> T.readFile path)
                 (runValidationT . inputErrorHandling filePath)
        if   onull txt
        then invalid $ makeEmptyFileStream filePath
        else pure txt


-- |
-- Smartly handle certain I/O errors that can occur while inputing a data stream.
--
-- Re-throws errors not specially handled and reported by 'InputStreamError'.
inputErrorHandling :: FileSource -> IOError -> ValidationT InputStreamError IO a
inputErrorHandling filePath e
  | isAlreadyInUseError e = invalid $ makeFileInUseOnRead       filePath
  | isPermissionError   e = invalid $ makeFileNoReadPermissions filePath
  | isDoesNotExistError e = invalid $ makeFileNotFound          filePath
  -- Re-throw if it is not an error we explicitly handle and report
  | otherwise             = ValidationT $ ioError e


-- |
-- Smartly handle certain I/O errors that can occur while outputing a data stream.
--
-- Re-throws errors not specially handled and reported by 'OutputStreamError'.
outputErrorHandling :: FileSource -> IOError -> ValidationT OutputStreamError IO a
outputErrorHandling filePath e
  | isAlreadyInUseError e = invalid $ makeFileInUseOnWrite       filePath
  | isFullError         e = invalid $ makeNotEnoughSpace         filePath
  | isPermissionError   e = invalid $ makeFileNoWritePermissions filePath
  | isDoesNotExistError e = invalid $ makePathDoesNotExist       filePath
  -- Re-throw if it is not an error we explicitly handle and report
  | otherwise             = ValidationT $ ioError e


-- |
-- Checks to see if the supplied file path exists.
--
-- If it does, it moves the existing file path, so that the supplied file path
-- can be written to without overwriting data.
--
-- The exisiting file path is renamed, adding a numeric suffix to the end. The
-- function will try to rename the existing file path by adding the suffix ".0",
-- however if that filepath also exists, it will add ".1", ".2", ".3", ",.4", etc.
-- The suffix added will be one greater than the highest existing numeric suffix.
safelyMoveFile :: FileSource -> IO ()
safelyMoveFile fs = do
    exists <- doesFileExist fp
    when exists $ do
        absPath <- makeAbsolute fp
        -- TODO: this logic is wrong,
        -- The file we write to might not be in the current directory!!!
        allFiles <- getDirectoryContents $ takeDirectory absPath
        let prefixed = getFilePathPrefixes     allFiles
        let numbers  = getNumericSuffixes      prefixed
        let lastNum  = getLargestNumericSuffix numbers
        let nextNum  = lastNum + 1 :: Word
        let newName  = absPath <> "." <> show nextNum
        renameFile absPath newName
  where
    fp = otoList fs

    getFilePathPrefixes = fmap (drop (length fp)) . filter (fp `isPrefixOf`)

    getNumericSuffixes  = fmap tail . filter hasDotThenNumberSuffix . fmap takeExtension
      where
        hasDotThenNumberSuffix ('.':x:xs) = all isNumber $ x:xs
        hasDotThenNumberSuffix _          = False

    getLargestNumericSuffix    []  = -1
    getLargestNumericSuffix (x:xs) = maximum . fmap read $ x:|xs
