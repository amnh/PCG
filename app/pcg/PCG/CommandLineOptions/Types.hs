------------------------------------------------------------------------------
-- |
-- Module      :  PCG.CommandLineOptions.Types
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

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
    } deriving stock (Generic)


-- |
-- Enumeration of verbosity levels.
data Verbosity
   = None
   | Errors
   | Warnings
   | Informational
   | Debugging
   deriving stock (Eq, Enum, Generic, Show)


instance NFData CommandLineOptions


instance NFData Verbosity


-- |
-- Interpret an 'Integer' as a 'Verbosity' value.
--
-- 'Integer' values in the range @[0 .. 4]@ are valid.
-- Values oput side the range default to @3@.
validateVerbosity :: Integer -> Verbosity
validateVerbosity 0 = None
validateVerbosity 1 = Errors
validateVerbosity 2 = Warnings
validateVerbosity 4 = Debugging
validateVerbosity _ = Informational
