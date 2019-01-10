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
-- Provides the types fot the \"REPORT\" command allong with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedSums #-}

module PCG.Command.Report
  ( ReportCommand(..)
  , OutputFormat(..)
  , OutputTarget(..)
  , FileWriteMethod(..)
  , reportCommandSpecification
  ) where


import Control.Applicative.Free
import Data.Functor             (($>))
import PCG.Syntax.Combinators


-- |
-- The \"REPORT\" command specifying what information should be output and where the
-- output should be directed.
data  ReportCommand
    = ReportCommand !OutputFormat !OutputTarget
    deriving (Show)


-- |
-- How the output should be formatted
data  OutputFormat
    = CrossReferences ![FilePath]
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
    | OutputToFile !FilePath !FileWriteMethod
    deriving (Show)


-- |
-- Defines the writing mode for how files should be written to disk.
data  FileWriteMethod
    = Append
    | Overwrite
    deriving (Eq, Show)


-- |
-- Defines the semantics of interpreting a valid \"REPORT\" command from the PCG
-- scripting language syntax.
reportCommandSpecification :: CommandSpecification ReportCommand
reportCommandSpecification = command "report" . argList $ ReportCommand <$> outputFormat <*> outputTarget


outputFormat :: Ap SyntacticArgument OutputFormat
outputFormat = choiceFrom [ dataFormat, dotFormat, xmlFormat, metadataFormat ]
  where
    dataFormat     = value "data" $> Data
    xmlFormat      = value "xml"  $> XML
    dotFormat      = choiceFrom [value "dot", value "graphviz"]  $> DotFile
    metadataFormat = choiceFrom [value "metadata", value "crossreferences"] $> Metadata


outputTarget :: Ap SyntacticArgument OutputTarget
outputTarget = choiceFrom [ stdout, toFile ] `withDefault` OutputToStdout
  where
    stdout = value "stdout" $> OutputToStdout
    toFile = choiceFrom
        [ argList $ OutputToFile <$> text <*> fileWriteMethod
        ,           OutputToFile <$> text <*> pure Append
        ]


fileWriteMethod :: Ap SyntacticArgument FileWriteMethod
fileWriteMethod = choiceFrom
    [ value "overwrite" $> Overwrite
    , value "append"    $> Append
    ] `withDefault` Append
