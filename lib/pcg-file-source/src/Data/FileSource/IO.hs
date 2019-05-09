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
-- Exposes data input utility which more granualarly controls I/O.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FileSource.IO
  ( -- * Inputing Text
    readFile
  , readFiles
  , readSTDIN
    -- * Output Streams
  , FileStream()
  , streamBytes
  , streamText
  , appendFile
  , writeFile
  , writeFileWithMove
  , writeSTDOUT
    -- * Binary data I/O
  , deserializeBinary
  , serializeBinary
    -- * Compact region I/O
  , deserializeCompact
  , serializeCompact
    -- * Error types of I/O
  , InputStreamError()
  , OutputStreamError()
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Validation
import           Data.Bifunctor
import           Data.Binary                       (Binary, decodeFileOrFail, encode)
import           Data.ByteString.Lazy              (ByteString)
import qualified Data.ByteString.Lazy              as BS
import           Data.Char                         (isNumber)
import           Data.Compact                      (Compact)
import           Data.Compact.Serialize            (hPutCompact, unsafeReadCompact)
import           Data.FileSource
import           Data.FileSource.InputStreamError
import           Data.FileSource.OutputStreamError
import           Data.FileSource.ParseStreamError
import           Data.Foldable
import           Data.List                         (isPrefixOf)
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.MonoTraversable
import           Data.String
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy.IO                 as T
import           Data.Typeable                     (Typeable)
import           Data.Validation
import           Pipes                             (await, for, runEffect, yield, (>~))
import           Prelude                           hiding (appendFile, getContents, readFile, writeFile)
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix             (takeDirectory, takeExtension)
import           System.IO                         hiding (appendFile, putStrLn, readFile, writeFile)
import           System.IO.Error
import           TextShow                          (printT)


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
readSTDIN :: ValidationT InputStreamError IO Text
readSTDIN = do
    nonEmptyStream <- liftIO $ hReady stdin
    if   nonEmptyStream
    then liftIO . runEffect $ liftIO T.getContents >~ await
    else invalid $ makeEmptyFileStream "STDIN"


-- |
-- Represents a stream of data, either textual or of raw byte data.
--
-- A 'FileStream' will be lazily rendered to it's output source in constant memory.
--
-- Create a 'FileStream' with
--
--   * 'streamBytes'
--   * 'streamText'
--
-- Render a stream with
--
--   * 'appendFile'
--   * 'writeFile'
--   * 'writeFileWithMove'
--   * 'writeSTDOUT'
--
data  FileStream
    = T Text
    | B ByteString


-- |
-- Convert a /lazy/ 'Text' stream to be used in output streaming functions.
{-# INLINE streamText #-}
streamText :: Text -> FileStream
streamText = T


-- |
-- Convert a /lazy/ 'ByteString' stream to be used in output streaming functions.
{-# INLINE streamBytes #-}
streamBytes :: ByteString -> FileStream
streamBytes = B


-- |
-- Write textual stream to the /end/ of the file.
appendFile :: FileSource -> FileStream -> ValidationT OutputStreamError IO ()
appendFile filePath str = ValidationT $ catch
      (Success <$> streamToFile AppendMode filePath str)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Write textual stream to the to the file, overwriting any exisiting file contents.
writeFile :: FileSource -> FileStream -> ValidationT OutputStreamError IO ()
writeFile filePath str = ValidationT $ catch
      (Success <$> streamToFile WriteMode filePath str)
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
writeFileWithMove :: FileSource -> FileStream -> ValidationT OutputStreamError IO ()
writeFileWithMove filePath str = liftIO (safelyMoveFile filePath) *> writeFile filePath str


-- |
-- Render the stream to STDIN.
writeSTDOUT :: FileStream -> ValidationT OutputStreamError IO ()
writeSTDOUT = liftIO . \case
    T s -> T.putStr s
    B s -> BS.putStr s


-- |
-- Deserialize binary encodable content from the specified file path.
--
-- Operational inverse of 'serializeBinary'.
deserializeBinary :: Binary a => FileSource -> ValidationT (Either InputStreamError ParseStreamError) IO a
deserializeBinary filePath =
    readFilesAndLocate'
      Left
      deserialize
      (invalid . Left . makeAmbiguousFiles filePath)
      filePath
  where
    deserialize fp = do
        res <- ValidationT $ catch
                   (Success <$> decodeFileOrFail (otoList fp))
                   (fmap (first Left) . runValidationT . inputErrorHandling fp)
        case res of
          Left  (_, err) -> invalid . Right . makeDeserializeErrorInBinaryEncoding fp $ fromString err
          Right val      -> pure val


-- |
-- Serialize binary encodable content to the specified file path.
--
-- Operational inverse of 'deserializeBinary'.
serializeBinary :: Binary a => FileSource -> a -> ValidationT OutputStreamError IO ()
serializeBinary filePath val =
    ValidationT $ catch
      (fmap Success . streamToFile WriteMode filePath . streamBytes $ encode val)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Deserialize a compact region from the specified file path.
--
-- Operational inverse of 'serializeCompact'.
deserializeCompact :: Typeable a => FileSource -> ValidationT (Either InputStreamError ParseStreamError) IO (Compact a)
deserializeCompact filePath =
    readFilesAndLocate'
      Left
      deserialize
      (invalid . Left . makeAmbiguousFiles filePath)
      filePath
  where
    deserialize fp = do
        res <- ValidationT $ catch
                   (Success <$> unsafeReadCompact (otoList fp))
                   (fmap (first Left) . runValidationT . inputErrorHandling fp)
        case res of
          Left  err -> invalid . Right . makeDeserializeErrorInCompactRegion fp $ fromString err
          Right val -> pure val


-- |
-- Serialize a compact region's contents to the specified file path.
--
-- Operational inverse of 'deserializeCompact'.
serializeCompact :: Typeable a => FileSource -> Compact a -> ValidationT OutputStreamError IO ()
serializeCompact filePath val =
    ValidationT $ catch
      (Success <$> runStream hPutCompact WriteMode filePath val)
      (runValidationT . outputErrorHandling filePath)


-- |
-- Read the textual contents of one or more files matching a "file globbing" pattern.
readFilesAndLocate
  :: (FileSource -> ValidationT InputStreamError IO a)          -- ^ What to do in the unambiguous case
  -> (NonEmpty FileSource -> ValidationT InputStreamError IO a) -- ^ What to do in the ambiguous case
  -> FileSource -- ^ Path description
  -> ValidationT InputStreamError IO a -- ^ Result
readFilesAndLocate = readFilesAndLocate' id


-- |
-- Read the textual contents of one or more files matching a "file globbing" pattern.
readFilesAndLocate'
  :: Semigroup e
  => (InputStreamError -> e)                     -- ^ How to project an input error to the type e
  -> (FileSource -> ValidationT e IO a)          -- ^ What to do in the unambiguous case
  -> (NonEmpty FileSource -> ValidationT e IO a) -- ^ What to do in the ambiguous case
  -> FileSource -- ^ Path description
  -> ValidationT e IO a -- ^ Result
readFilesAndLocate' e f g filePath = do
    -- Check if the file exists exactly as specified
    exists <- liftIO $ doesFileExist (otoList filePath)
    if exists
    -- If it exists exactly as specified, read it in
    then f filePath
    else do
        -- If the file does not exists exactly as specified
        -- try to match other files to the given path
        -- by interpreting the path as a 'glob'
        matches <- fmap (fmap fromString) . liftIO . glob $ otoList filePath
        case matches of
          []   -> invalid . e $ makeFileNotFound filePath
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
                 (Success <$> streamfromFile filePath)
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
  | otherwise = ValidationT $ ioError e


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
  | otherwise = ValidationT $ ioError e


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



-- |
-- Streams text to a file in constant memory for any type with a `TextShow`
-- instance.
streamfromFile :: FileSource -> IO Text
streamfromFile = T.readFile . otoList
-- This streaming from file doesn't work...
{-
streamfromFile filePath = do
    h   <- openFile (otoList filePath) ReadMode
    txt <- runEffect $ (liftIO (T.hGetLine h)) >~ await
    hClose h
    pure txt
-}

-- |
-- Streams text to a file in constant memory for any type with a `TextShow`
-- instance.
streamToFile :: IOMode -> FileSource -> FileStream -> IO ()
streamToFile m fs = \case T txt -> runStream  T.hPutStr m fs txt
                          B bts -> runStream BS.hPutStr m fs bts


-- |
-- Given a streaming function to a file handle, write out a data stream.
runStream :: (Handle -> v -> IO ()) -> IOMode -> FileSource -> v -> IO ()
runStream f mode fp v = do
    h <- openFile (otoList fp) mode
    runEffect $ for (yield v) (liftIO . f h)
    hClose h
