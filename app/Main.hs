{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main (main) where

import Control.DeepSeq
import Control.Evaluation
import Control.Monad.Reader               (runReaderT)
import Data.Char                          (toUpper)
import Data.Semigroup                     ((<>))
import Data.Version                       (showVersion)
import Data.Void
import Development.GitRev                 (gitCommitCount, gitHash)
import GHC.Generics
import Options.Applicative                hiding (ParseError)
import Paths_phylogenetic_component_graph (version)
import PCG.Computation.Internal
import PCG.Syntax                         (computationalStreamParser)
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec                    (ParseErrorBundle, Parsec, errorBundlePretty, parse)
import Text.PrettyPrint.ANSI.Leijen       (align, indent, int, line, string, text, (<+>), (</>))


-- |
-- Valid command line options
data  CommandLineOptions
    = CommandLineOptions
    { inputFile    :: FilePath
    , outputFile   :: FilePath
    , printVersion :: Bool
    , printSplash  :: Bool
    , verbosity    :: Verbosity
    } deriving (Generic)


-- |
-- Enumeration of verbosity levels.
data Verbosity
   = None
   | Errors
   | Warnings
   | Informational
   | Debugging
   deriving (Eq, Enum, Generic, Show)


instance NFData CommandLineOptions


instance NFData Verbosity


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
     globalSettings <- getGlobalSettings
     let  _verbosity = verbosity opts
     when (printSplash opts) printSplashImage
     if   printVersion opts
     then putStrLn fullVersionInformation
     else do
          inputStreamMaybe <- retreiveInputStream $ inputFile opts
          case inputStreamMaybe of
            Left errorMessage -> putStrLn errorMessage
            Right inputStream -> do
                (code, outputStream) <- case parse' computationalStreamParser (inputFile opts) inputStream of
                                          Left  err -> pure (ExitFailure 4, errorBundlePretty err)
                                          Right val -> fmap renderSearchState . (`runReaderT` globalSettings) . runEvaluation . evaluate $ optimizeComputation val
                let  outputPath = outputFile opts
                if   (toUpper <$> outputPath) == "STDOUT"
                then hSetBuffering stdout NoBuffering >> putStrLn outputStream
                else writeFile outputPath outputStream
                exitWith code
  where
     parse' :: Parsec Void s a -> String -> s -> Either (ParseErrorBundle s Void) a
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
-- stream was intentionally choosen as the input stream and an error message
-- noting that the stream is empty is returned along with the program's usage
-- menu.
retreiveInputStream :: FilePath -> IO (Either String String)
retreiveInputStream path
  | (toUpper <$> path) /= "STDIN" = Right <$> readFile path
  | otherwise = do
      nonEmptyStream <- hReady stdin
      if   nonEmptyStream
      then Right <$> getContents
      else do
           args <- getArgs
           if   null args
           then Left <$> parserHelpMessage
           else Left . ("Error: STDIN is empty\n\n" <>) <$> parserHelpMessage


-- |
-- Generates the program's help menu based on the command line options parser.
parserHelpMessage :: IO String
parserHelpMessage = fst . renderFailure failValue <$> getProgName
  where
    failValue = parserFailure (prefs showHelpOnEmpty) parserInformation ShowHelpText []


-- |
-- Command to parse the command line options.
parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = customExecParser preferences parserInformation
  where
    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]


-- |
-- Information regarding which command line options are valid and how they are
-- parsed and interpreted.
parserInformation :: ParserInfo CommandLineOptions
parserInformation = info (helper <*> commandLineOptions) description
  where
    commandLineOptions =
        CommandLineOptions
          <$> fileSpec 'i' "input"  "STDIN"  "Input PCG script file"
          <*> fileSpec 'o' "output" "STDOUT" "Output file"
          <*> switch  (mconcat [long "version", help "Display version number"])
          <*> switch  (mconcat [long "splash" , help "Display splash image"])
          <*> (validateVerbosity <$> option auto verbositySpec)

    fileSpec c s d h = strOption $ mconcat
        [ short c
        , long  s
        , value d
        , help  $ mconcat [h, " (default ", d, ")"]
        , metavar "FILE"
        ]

    verbositySpec = mconcat
        [ short 'v'
        , long "verbosity"
        , value 3
        , helpDoc verbosityHelp
        , metavar "LEVEL"
        ]

    description = mconcat
        [ fullDesc
        , headerDoc . Just . string $ "  " <> softwareName <> "\n  " <> shortVersionInformation
        ]

    verbosityHelp = Just . (text "Select the verbosity level (default 3):" `op`) . indent 2 . foldl1 op $ f <$>
        [ (0, ["Suppress all output"])
        , (1, ["Output only errors"])
        , (2, ["Output errors and", "warnings"])
        , (3, ["Output errors,", "warnings,", "and runtime information"])
        , (4, ["Output errors,", "warnings,", "runtime information,", "and debugging information"])
        ]
      where
        f (i, x) = int i <+> align (g x)
        g = foldl1 (</>) . fmap text
        op x y = x <> line <> y


-- |
-- Name of the software package.
softwareName :: String
softwareName = "Phylogenetic Component Graph"


-- |
-- Brief description of the software version.
shortVersionInformation :: String
shortVersionInformation = "(alpha) version " <> showVersion version


-- |
-- Full escription of the software version.
--
-- Uses @TemplateHaskell@ to splice in git hash and commit count information
-- from the compilation environment.
fullVersionInformation :: String
fullVersionInformation = mconcat
    [ softwareName
    , " "
    , shortVersionInformation
    , " ["
    , take 7 $(gitHash)
    , "] ("
    , $(gitCommitCount)
    , " commits)"
    ]


-- |
-- Interpret an 'Integer' as a 'Verbosity' value.
--
-- 'Integer' values in the range @[0 .. 4]@ are valid.
-- Values oput side the range defualt to @3@.
validateVerbosity :: Integer -> Verbosity
validateVerbosity 0 = None
validateVerbosity 1 = Errors
validateVerbosity 2 = Warnings
validateVerbosity 4 = Debugging
validateVerbosity _ = Informational


printSplashImage :: IO ()
printSplashImage = putStrLn $ unlines
  [ "______ _           _                             _   _      "
  , "| ___ \\ |         | |                           | | (_)     "
  , "| |_/ / |__  _   _| | ___   __ _  ___ _ __   ___| |_ _  ___ "
  , "|  __/| '_ \\| | | | |/ _ \\ / _` |/ _ \\ '_ \\ / _ \\ __| |/ __|"
  , "| |   | | | | |_| | | (_) | (_| |  __/ | | |  __/ |_| | (__ "
  , "\\_|   |_| |_|\\__, |_|\\___/ \\__, |\\___|_| |_|\\___|\\__|_|\\___|"
  , "              __/ |         __/ |                           "
  , "             |___/         |___/                            "
  , " _____                                              _       "
  , "/  __ \\                                            | |      "
  , "| /  \\/ ___  _ __ ___  _ __   ___  _ __   ___ _ __ | |_     "
  , "| |    / _ \\| '_ ` _ \\| '_ \\ / _ \\| '_ \\ / _ \\ '_ \\| __|    "
  , "| \\__/\\ (_) | | | | | | |_) | (_) | | | |  __/ | | | |_     "
  , " \\____/\\___/|_| |_| |_| .__/ \\___/|_| |_|\\___|_| |_|\\__|    "
  , "                      | |                                   "
  , "                      |_|                                   "
  , " _____                 _                                    "
  , "|  __ \\               | |                                   "
  , "| |  \\/_ __ __ _ _ __ | |__  ___                            "
  , "| | __| '__/ _` | '_ \\| '_ \\/ __|                           "
  , "| |_\ \ | | (_| | |_) | | | \\__ \\                           "
  , " \\____/_|  \\__,_| .__/|_| |_|___/                           "
  , "                | |                                         "
  , "                |_|                                         "
  ]
