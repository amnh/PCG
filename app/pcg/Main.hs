-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The entry point to the PCG executable.
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq
import Control.Evaluation
import Control.Exception              (catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Validation
import Data.Char                      (toUpper)
import Data.FileSource                (FileSource)
import Data.FileSource.IO
import Data.Foldable
import Data.Maybe
import Data.MonoTraversable
import Data.String                    (fromString)
import Data.Text.Lazy                 (Text, pack, unlines)
import Data.Text.Lazy.IO              (putStr, putStrLn)
import Data.Validation
import Data.Void
import PCG.CommandLineOptions
import PCG.Computation.Internal
import PCG.Syntax                     (Computation, computationalStreamParser)
import Prelude                        hiding (putStr, putStrLn, readFile, unlines, writeFile)
import System.Environment
import System.Exit
import System.IO                      hiding (putStr, putStrLn, readFile, writeFile)
import System.IO.Error
import Text.Megaparsec                (ParseErrorBundle, Parsec, errorBundlePretty, parse)


-- |
-- Main evaluation call.
--
-- Parses command line options, handles parse errors gracefully.
--
-- Conditionally prints version or help information and exits when requested.
--
-- Gracefully handles empty STDIN stream.
--
-- Initiates phylogenetic search when valid command line options are supplied.
main :: IO ()
main = do
     hSetEncoding stdout utf8
     hSetEncoding stderr utf8
     handleNoInput
     opts <- force <$> parseCommandLineOptions
     let  _verbosity = verbosity opts
     fromMaybe (performSearch opts) $ gatherDisplayInformation opts


-- |
-- First we check if STDIN is empty and there were no command line arguments.
-- In this case of "no inputs," print the help screen and exit.
-- Otherwise proceed as normal.
handleNoInput :: IO ()
handleNoInput = do
    noArguments  <- null <$> getArgs
    stdinIsEmpty <- not  <$> nonEmptySTDIN
    when (noArguments && stdinIsEmpty) $ do
      parserHelpMessage >>= putStrLn . fromString
      exitSuccess


performSearch :: CommandLineOptions -> IO ()
performSearch opts = do
    (code, outputStream) <- runUserComputation opts
    let outputPath = outputFile opts
    (code2, _) <- fmap (renderSearchState . snd) . runEvaluationT () $ renderOutputStream outputPath outputStream
    -- If the computation was successful and the outputting was unsuccessful,
    -- only then use the exit code generated during outputting.
    case code of
      ExitSuccess{} -> exitWith code2
      _             -> exitWith code


runUserComputation :: CommandLineOptions -> IO (ExitCode, Text)
runUserComputation opts = do
    globalSettings  <- liftIO getGlobalSettings
    (notes, result) <- runEvaluationT globalSettings computationalEvaluation
    putStr $ renderedNotifications notes
    pure $ renderSearchState result
  where
    computationalEvaluation = do
        inputStream    <- retreiveInputStream $ inputFile opts
        computation    <- parseInputStream (inputFile opts) inputStream
        evaluate computation

    renderedNotifications = unlines . toList . fmap f
      where
        f :: Notification -> Text
        f (Information s) = "[-] " <> s
        f (Warning     s) = "[!] " <> s


renderOutputStream :: FileSource -> Text -> EvaluationT r IO ()
renderOutputStream filePath outputStream = do
    result <- liftIO $ if   (toUpper <$> otoList filePath) /= "STDOUT"
                       then runValidationT . writeFile filePath $ streamText outputStream
                       else hSetBuffering stdout NoBuffering *>
                            runValidationT (writeSTDOUT (streamText outputStream))
    case result of
      Failure err -> failWithPhase Outputting err
      Success _   -> pure ()


parseInputStream :: FileSource -> Text -> EvaluationT r IO Computation
parseInputStream path inputStream =
   case parse' computationalStreamParser (otoList path) inputStream of
     Left  err -> failWithPhase Parsing . pack $ '\n' : errorBundlePretty err
     Right val -> pure $ optimizeComputation val
  where
     parse'
       :: Parsec Void Text Computation
       -> FilePath
       -> Text
       -> Either (ParseErrorBundle Text Void) Computation
     parse' = parse


-- |
-- Attempts to read from the FilePath.
--
-- If the 'FilePath' is *not* STDIN or the 'FilePath' is STDIN and the STIDN
-- stream is non-empty, then a success value ('Right') is returned.
--
-- If the 'FilePath' is STDIN and the STDIN stream is empty, then an error value
-- ('Left') is returned. In the error case where no arguments were supplied to
-- the program, then the help menu is returned as the error message. In the error
-- case where program arguments were supplied, it is assumed that the STDIN
-- stream was intentionally chosen as the input stream and an error message
-- noting that the stream is empty is returned along with the program's usage
-- menu.
retreiveInputStream :: FileSource -> EvaluationT r IO Text
retreiveInputStream filePath = do
    inResult <- liftIO . runValidationT $ getInputStream filePath
    case inResult of
      Failure err -> failWithPhase Inputing err
      Success may ->
        case may of
          Just  v -> pure v
          Nothing -> do
              msg <- liftIO parserHelpMessage
              failWithPhase Inputing $ "Error: STDIN is empty\n\n" <> msg
  where
    getInputStream :: FileSource -> ValidationT InputStreamError IO (Maybe Text)
    getInputStream path
      | (toUpper <$> otoList path) /= "STDIN" = Just <$> readFile path
      | otherwise = do
          isNonEmpty <- nonEmptySTDIN
          if   isNonEmpty
          then Just <$> readSTDIN
          else pure Nothing


nonEmptySTDIN :: MonadIO m => m Bool
nonEmptySTDIN = liftIO $ catch (hReady stdin) errorHandling
  where
    errorHandling :: IOError -> IO Bool
    errorHandling e
      | isEOFError e || isDoesNotExistError e = pure False
      | otherwise = ioError e
