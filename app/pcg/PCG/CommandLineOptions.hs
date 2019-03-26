{-# LANGUAGE FlexibleContexts #-}

module PCG.CommandLineOptions
  ( -- * Types
    CommandLineOptions(..)
  , Verbosity()
    -- * Parser
  , parseCommandLineOptions
  , parserHelpMessage
    -- * Display information
  , gatherDisplayInformation
  ) where

import Data.Foldable
import Data.Semigroup                 ((<>))
import Options.Applicative            hiding (ParseError)
import PCG.CommandLineOptions.Display
import PCG.CommandLineOptions.Types
import PCG.Software.Metadata
import System.Environment
import Text.PrettyPrint.ANSI.Leijen   (align, indent, int, line, string, text, (<+>), (</>))


-- |
-- Command to parse the command line options.
parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = customExecParser preferences parserInformation
  where
    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]


-- |
-- Generates the program's help menu based on the command line options parser.
parserHelpMessage :: IO String
parserHelpMessage = fst . renderFailure failValue <$> getProgName
  where
    failValue = parserFailure (prefs showHelpOnEmpty) parserInformation ShowHelpText []


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
          <*> switch  (fold [long "version", help "Display version number"])
          <*> switch  (fold [long "splash" , help "Display splash image"])
          <*> switch  (fold [long "credits", help "Display project contributions"])
          <*> (validateVerbosity <$> option auto verbositySpec)

    fileSpec c s d h = strOption $ fold
        [ short c
        , long  s
        , value d
        , help  $ fold [h, " (default ", d, ")"]
        , metavar "FILE"
        ]

    verbositySpec = fold
        [ short 'v'
        , long "verbosity"
        , value 3
        , helpDoc verbosityHelp
        , metavar "LEVEL"
        ]

    description = fold
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
