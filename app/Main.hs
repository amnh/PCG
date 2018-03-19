{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Main (main) where


import Paths_phylocomgraph (version)
import Control.DeepSeq
import Control.Evaluation
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Data.Void
import GHC.Generics
import Options.Applicative hiding (ParseError)
import PCG.Computation.Internal
import PCG.Syntax (computationalStreamParser)
import System.IO
import Text.Megaparsec (Parsec, ParseError, Token, parse, parseErrorPretty')
import Text.PrettyPrint.ANSI.Leijen ((<+>), (</>), align, indent, int, line, string, text)


data CommandLineOptions
   = CommandLineOptions
   { printVersion :: Bool
   , inputFile    :: String
   , outputFile   :: String
   , verbosityNum :: Integer -- Curently unused
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
     hSetBuffering stdout NoBuffering
     opts <- parseCommandLineOptions
     let  verbosity = validateVerbosity $ verbosityNum opts
     if   printVersion opts
     then putStrLn $ "Phylogenetic Component Graph alpha version " <> showVersion version
     else do
          inputStream  <- if   inputFile opts == "STDIN"
                          then getContents
                          else readFile $ inputFile opts
          outputStream <- case parse' computationalStreamParser (inputFile opts) inputStream of
                            Left  err -> pure $ parseErrorPretty' (inputFile opts) err
                            Right val -> renderSearchState <$> runEvaluation (evaluate (optimizeComputation val))
          if   outputFile opts == "STDOUT"
          then putStrLn outputStream
          else writeFile (outputFile opts) outputStream
  where
     parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
     parse' = parse
      

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = customExecParser preferences $ info (helper <*> commandLineOptions) description
  where
    commandLineOptions =
        CommandLineOptions
          <$> switch  (mconcat [long "version", help "Display version number"])
          <*> fileSpec 'i' "input"  "STDIN"  "Input PCG script file, defaults to STDIN"
          <*> fileSpec 'o' "output" "STDOUT" "Output file, defaults to STDOUT"
          <*> (toEnum <$> option auto (mconcat [short 'v', long "verbosity", value 3, helpDoc verbosityHelp]))

    fileSpec c s d h = strOption $ mconcat [short c, long s, value d, help h, metavar "FILE"]

    description = mconcat
        [ fullDesc
        , headerDoc (Just (string "\n  Phylogenetic Component Graph"))
        , footerDoc (Just mempty)
        ]

    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]

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

