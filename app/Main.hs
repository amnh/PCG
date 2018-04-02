{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Main (main) where


import Paths_phylocomgraph          (version)
import Control.DeepSeq
import Control.Evaluation
import Data.Char                    (toUpper)
import Data.Semigroup               ((<>))
import Data.Version                 (showVersion)
import Data.Void
import Development.GitRev           (gitCommitCount, gitHash)
import GHC.Generics
import Options.Applicative   hiding (ParseError)
import PCG.Computation.Internal
import PCG.Syntax                   (computationalStreamParser)
import System.Environment
import System.IO
import Text.Megaparsec              (Parsec, ParseError, Token, parse, parseErrorPretty')
import Text.PrettyPrint.ANSI.Leijen ((<+>), (</>), align, indent, int, line, string, text)


data  CommandLineOptions
    = CommandLineOptions
    { inputFile    :: FilePath
    , outputFile   :: FilePath
    , printVersion :: Bool
    , verbosity    :: Verbosity
    } deriving (Generic)


data Verbosity
   = None
   | Errors
   | Warnings
   | Informational
   | Debugging
   deriving (Eq, Enum, Generic, Show)


instance NFData CommandLineOptions


instance NFData Verbosity


main :: IO ()
main = do
     opts <- force <$> parseCommandLineOptions
     let  _verbosity = verbosity opts
     if   printVersion opts
     then putStrLn fullVersionInformation
     else do
          inputStreamMaybe <- retreiveInputStream $ inputFile opts
          case inputStreamMaybe of
            Nothing -> parserHelpMessage >>= putStrLn
            Just inputStream -> do
                outputStream <- case parse' computationalStreamParser (inputFile opts) inputStream of
                                  Left  err -> pure $ parseErrorPretty' (inputFile opts) err
                                  Right val -> renderSearchState <$> runEvaluation (evaluate (optimizeComputation val))
                if   outputFile opts == "STDOUT"
                then hSetBuffering stdout NoBuffering >> putStrLn outputStream
                else writeFile (outputFile opts) outputStream
  where
     parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
     parse' = parse


-- |
-- Attempts to read from the FilePath.
--
-- If the FilePath is STDIN  and no arguments were supplied to the program,
-- then an IO failure is returned instead of a String value. The IO failure
-- prints the program's help menu. This creates the effect that when no arguments
-- are supplied to the program, i prints the help menu.
retreiveInputStream :: FilePath -> IO (Maybe String)
retreiveInputStream path
  | (toUpper <$> path) /= "STDIN" = Just <$> readFile path
  | otherwise = do
      args <- getArgs
      if   null args 
      then do
          emptySTDIN <- not <$> hReady stdin
          if   emptySTDIN
          then pure Nothing
          else Just <$> getContents
      else Just <$> getContents


softwareName :: String
softwareName = "Phylogenetic Component Graph"


shortVersionInformation :: String
shortVersionInformation = "(alpha) version " <> showVersion version


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
  

parserHelpMessage :: IO String
parserHelpMessage = do
    name <- getProgName
    pure . fst . (`renderFailure` name) $ parserFailure (prefs showHelpOnEmpty) parserInformation ShowHelpText []


parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = customExecParser preferences parserInformation
  where
    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]


parserInformation :: ParserInfo CommandLineOptions
parserInformation = info (helper <*> commandLineOptions) description
  where
    commandLineOptions =
        CommandLineOptions
          <$> fileSpec 'i' "input"  "STDIN"  "Input PCG script file"
          <*> fileSpec 'o' "output" "STDOUT" "Output file"
          <*> switch  (mconcat [long "version", help "Display version number"])
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
--        , footerDoc $ Just mempty
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


validateVerbosity :: Integer -> Verbosity
validateVerbosity 0 = None
validateVerbosity 1 = Errors
validateVerbosity 2 = Warnings
validateVerbosity 4 = Debugging
validateVerbosity _ = Informational

