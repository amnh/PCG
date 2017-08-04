-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Report
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types fot the Report command allong with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

module PCG.Command.Report
  ( ReportCommand(..)
  , OutputFormat(..)
  , OutputTarget(..)
  , reportCommandSpecification
  ) where


import Control.Applicative.Free
import Data.Functor (($>))
import PCG.Syntax.Combinators


-- |
-- How the output should be formatted
data  OutputFormat
    = CrossReferences [FilePath]
    | Data
    | DotFile
    | DynamicTable
    | ImpliedAlignmentCharacters
    | LikelihoodModel
    | Metadata
    | SearchStats
    | SequenceStats
    | Terminals
    | TreeCosts
    | TreeStats
    | XML
    deriving (Show)


-- |
-- Where the output stream should be directed.
data  OutputTarget
    = OutputToStdout
    | OutputToFile FilePath FileWriteMethod
    deriving (Show)


data FileWritingMethod = Append | Overwrite deriving (Eq,Show)


-- |
-- The REPORT command specifying what information should be output and where the
-- output should be directed.
data  ReportCommand
    = ReportCommand OutputFormat OutputTarget
    deriving (Show)


-- |
-- Defines the semantics of interpreting a valid \"Report\" command from the PCG
-- scripting language syntax.
reportCommandSpecification :: CommandSpecification ReportCommand
reportCommandSpecification = command "report" . argList $ ReportCommand <$> outputFormat <*> outputTarget


outputTarget :: Ap SyntacticArgument OutputTarget
outputTarget = choiceFrom [ stdout, toFile ] `withDefault` OutputToStdout
  where
    stdout = value "stdout" $> OutputToStdout
    toFile = OutputToFile <$> text <*> fileWriteMethod


outputFormat :: Ap SyntacticArgument OutputFormat
outputFormat = choiceFrom [ dataFormat, dotFormat, xmlFormat ]
  where
    dataFormat = value "data" $> Data
    xmlFormat  = value "xml"  $> XML
    dotFormat  = choiceFrom [value "dot", value "graphviz"]  $> DotFile


fileWriteMethod = choiceFrom [ value "overwrite" $> Overwrite, value "append" $> Append ] `withDefault` Append
