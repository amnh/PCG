------------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.SubProcess
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}

module TestSuite.SubProcess
  ( ScriptContext(..)
  , collectFileContents
  , constructProcess
  ,  destructProcess
  , testDirectory
  ) where

import Control.Arrow         ((&&&))
import Control.DeepSeq
import Control.Monad         (when)
import Data.ByteString.Lazy  (ByteString, readFile)
import Data.Foldable
import Prelude               hiding (readFile)
import System.Directory
import System.FilePath.Posix
import System.IO             (hClose)
import System.Process


-- |
-- A data structure for storing the process and related paths.
data  ScriptContext
    = ScriptContext
    { process :: CreateProcess
    , outPath :: FilePath
    , errPath :: FilePath
    }


-- |
-- Default directory in which to find the executable for the processes.
binaryDirectory :: FilePath
binaryDirectory = "./bin/pcg"


-- |
-- Default directory in which to find integration tests.
testDirectory :: FilePath
testDirectory = "test" </> "data-sets"


-- |
-- Default STDOUT file name.
--
-- A process created by this module will not produce any terminal output.
-- All output will be redirdected to 'outLogFileName' and 'errLogFileName'.
outLogFileName :: FilePath
outLogFileName = "log" <.> "out"


-- |
-- Default STDERR file name.
--
-- All output will be redirdected to 'outLogFileName' and 'errLogFileName'.
errLogFileName :: FilePath
errLogFileName = "log" <.> "err"


-- |
-- Given a list of 'FilePath's, return a list of the file's contents.
collectFileContents :: Traversable t => t FilePath -> IO (t ByteString)
collectFileContents = traverse nicelyReadFile . fmap (testDirectory </>)
  where
   nicelyReadFile :: FilePath -> IO ByteString
   nicelyReadFile filePath = do
      fileExist   <- doesFileExist filePath
      absFilePath <- makeAbsolute  filePath
      if   fileExist
      then force <$> readFile filePath
      else fail $ unlines
                [ "No file found with the specified filepath:"
                , absFilePath
                ]


-- |
-- Takes a 'FilePath' to a PCG script and executes an instance of PCG using the
-- script as the process's input.
--
-- Call 'destructProcess' on the supplied 'ScriptContext' afterwards to clean up
-- artifacts of the process.
constructProcess
  :: FilePath -- ^ Relative path to the PCG script
  -> IO ScriptContext
constructProcess scriptStr = do
    prefix      <- makeAbsolute scriptDirectory
    binFilePath <- makeAbsolute binaryDirectory
    let runFilePath = prefix </> scriptFileName
    let outFilePath = prefix </> outLogFileName
    let errFilePath = prefix </> errLogFileName
    let commandStr  = unwords
                    [ binFilePath
                    , "--input"
                    , runFilePath
                    , "--output"
                    , outFilePath
                    , "2>"
                    , errFilePath
                    ]

    -- Delete log files if they exist
    _ <- deleteFileIfExists outFilePath
    _ <- deleteFileIfExists errFilePath

    let p = CreateProcess
            { cmdspec            = ShellCommand commandStr
            , cwd                = Just scriptDirectory
            , env                = Nothing
            -- Do not use the stream handles, they do not work with Tasty
            , std_in             = NoStream
            , std_out            = NoStream
            , std_err            = NoStream
            , close_fds          = True
            , create_group       = False
            , delegate_ctlc      = False
            , detach_console     = False
            , create_new_console = False
            , new_session        = False
            , child_group        = Nothing
            , child_user         = Nothing
            , use_process_jobs   = False
            }

    pure ScriptContext
        { process = p
        , outPath = outFilePath
        , errPath = errFilePath
        }
  where
    (scriptDirectory, scriptFileName) = breakScriptPath $ testDirectory </> scriptStr

    breakScriptPath = (normalise . foldl' (</>) defaultDirectory . init &&& last) . splitDirectories
      where
        defaultDirectory = "."

    deleteFileIfExists p = do
        fileExists <- doesFileExist p
        when fileExists $ removeFile p


-- |
-- Clean up after a call to 'constructProcess'.
destructProcess :: ScriptContext -> IO ()
destructProcess ctx = mapM_ cleanUpHandle $
    [ std_out . process &&& outPath
    , std_err . process &&& errPath
    ] <*> [ ctx ]
  where
    cleanUpHandle (s, p) = do
        -- If we have an opened handle, close it
        _ <- case s of
               UseHandle h -> hClose h
               _           -> pure ()
        fileExists <- doesFileExist p
        when fileExists $ do
            n <- getFileSize p
            if n == 0 -- If no data was written, delete the file
            then removeFile p
            else pure ()
