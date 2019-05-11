{-# LANGUAGE DeriveGeneric #-}

module PCG.CommandLineOptions.Types
  ( CommandLineOptions(..)
  , Verbosity()
  , validateVerbosity
  ) where

import Control.DeepSeq
import Data.FileSource
import GHC.Generics


-- |
-- Valid command line options
data  CommandLineOptions
    = CommandLineOptions
    { inputFile      :: FileSource
    , outputFile     :: FileSource
    , printVersion   :: Bool
    , printSplash    :: Bool
    , printCredits   :: Bool
    , printExitCodes :: Bool
    , verbosity      :: Verbosity
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
