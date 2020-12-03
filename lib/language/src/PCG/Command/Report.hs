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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Command.Report
  ( ReportCommand(..)
  , OutputFormat(..)
  , OutputTarget(..)
  , FileWriteMethod(..)
  , reportCommandSpecification
  ) where


import Control.Applicative.Free
import Data.FileSource
import Data.Functor             (($>))
import PCG.Syntax.Combinators


-- |
-- The \"REPORT\" command specifying what information should be output and where the
-- output should be directed.
data  ReportCommand
    = ReportCommand !OutputFormat !OutputTarget
    deriving stock (Show)


-- |
-- How the output should be formatted
data  OutputFormat
    = CrossReferences ![FilePath]
    | Data
    | DistanceMatrix
    | DotFile
    | DynamicTable
    | ImpliedAlignment
    | LikelihoodModel
    | Metadata
    | SearchStats
    | SequenceStats
    | Terminals
    | TreeCosts
    | TreeStats
    | XML
    deriving stock (Show)


-- |
-- Where the output stream should be directed.
data  OutputTarget
    = OutputToStdout
    | OutputToFile !FileSource !FileWriteMethod
    deriving stock (Show)


-- |
-- Defines the writing mode for how files should be written to disk.
data  FileWriteMethod
    = Append
    | Move
    | Overwrite
    deriving stock (Eq, Show)


-- |
-- Defines the semantics of interpreting a valid \"REPORT\" command from the PCG
-- scripting language syntax.
reportCommandSpecification :: CommandSpecification ReportCommand
reportCommandSpecification = command "report" . argList $ ReportCommand <$> outputFormat <*> outputTarget


outputFormat :: Ap SyntacticArgument OutputFormat
outputFormat =
    choiceFrom
        [ dataFormat
        , distanceMatrix
        , dotFormat
        , impliedAlignFormat
        , metadataFormat
        , xmlFormat
        ]
  where
    dataFormat         = value "data" $> Data
    dotFormat          = choiceFrom [value "dot", value "graphviz"] $> DotFile
    distanceMatrix     = value "distance" $> DistanceMatrix
    impliedAlignFormat = value "implied-alignment" $> ImpliedAlignment
    metadataFormat     = choiceFrom [value "metadata", value "cross-references"] $> Metadata
    xmlFormat          = value "xml"  $> XML


outputTarget :: Ap SyntacticArgument OutputTarget
outputTarget = choiceFrom [ stdout, toFile ] `withDefault` OutputToStdout
  where
    stdout = value "stdout" $> OutputToStdout
    toFile = choiceFrom
        [ argList $ OutputToFile <$> filePath <*> fileWriteMethod
        ,           OutputToFile <$> filePath <*> pure Move
        ]
    filePath :: Ap SyntacticArgument FileSource
    filePath = FileSource <$> text


fileWriteMethod :: Ap SyntacticArgument FileWriteMethod
fileWriteMethod = choiceFrom
    [ value "overwrite" $> Overwrite
    , value "move"      $> Move
    , value "append"    $> Append
    ] `withDefault` Move
