{-# LANGUAGE DoAndIfThenElse #-}

module PCG.Command.Types.Report.Internal where

type FileName = String

data OutputFormat
   = CrossReferences [FileName]
   | Data
   | DotFile
   | ImpliedAlignmentCharacters
   | LikelihoodModel
   | Metadata
   | SearchStats
   | SequenceStats
   | Terminals
   | TreeCosts
   | TreeStats
   deriving (Show)

data OutputTarget
   = OutputToStdout
   | OutputToFile FilePath
   deriving (Show)
