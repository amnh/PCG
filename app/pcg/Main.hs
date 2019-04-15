{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq
import Control.Evaluation
import Control.Monad.Trans.Reader
import Data.Char                  (toUpper)
import Data.Maybe
import Data.Semigroup             ((<>))
import Data.Text                  (Text, pack)
import Data.Text.IO               (putStrLn, writeFile)
import Data.Void
import PCG.CommandLineOptions
import PCG.Computation.Internal
import PCG.Syntax                 (computationalStreamParser)
import Prelude             hiding (putStrLn, writeFile)
import System.Environment
import System.Exit
import System.IO           hiding (putStrLn, writeFile)
import Text.Megaparsec            (ParseErrorBundle, Parsec, errorBundlePretty, parse)


-- |
-- Main evaluation call.
--
-- Parses command line options, handles parse errors gracefully.
--
-- Conditionally prints version or help information and exits when requested.
--
-- Gracefully handles empty STDIN stream.
--
-- Initiates phylogenetic search when valid commmand line options are supplied.
main :: IO ()
main = do
     opts <- force <$> parseCommandLineOptions
     let  _verbosity = verbosity opts
     fromMaybe (performSearch opts) $ gatherDisplayInformation opts
  where
     parse' :: Parsec Void s a -> String -> s -> Either (ParseErrorBundle s Void) a
     parse' = parse

     performSearch :: CommandLineOptions -> IO ()
     performSearch opts = do
       globalSettings   <- getGlobalSettings
       inputStreamMaybe <- retreiveInputStream $ inputFile opts
       case inputStreamMaybe of
         Left errorMessage -> putStrLn errorMessage
         Right inputStream -> do
             (code, outputStream) <- case parse' computationalStreamParser (inputFile opts) inputStream of
                                       Left  err -> pure (ExitFailure 4, pack $ errorBundlePretty err)
                                       Right val -> fmap renderSearchState . (`runReaderT` globalSettings) . runEvaluation . evaluate $ optimizeComputation val
             let  outputPath = outputFile opts
             if   (toUpper <$> outputPath) == "STDOUT"
             then hSetBuffering stdout NoBuffering >> putStrLn outputStream
             else writeFile outputPath outputStream
             print code
             exitWith code


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
-- stream was intentionally choosen as the input stream and an error message
-- noting that the stream is empty is returned along with the program's usage
-- menu.
retreiveInputStream :: FilePath -> IO (Either Text String)
retreiveInputStream path
  | (toUpper <$> path) /= "STDIN" = Right <$> readFile path
  | otherwise = do
      nonEmptyStream <- hReady stdin
      if   nonEmptyStream
      then Right <$> getContents
      else do
           args <- getArgs
           if   null args
           then Left . pack <$> parserHelpMessage
           else Left . (\x -> "Error: STDIN is empty\n\n" <> pack x) <$> parserHelpMessage
