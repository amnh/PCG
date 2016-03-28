{-# LANGUAGE DoAndIfThenElse #-}

module PCG.Command.Types.Report.Internal where

type FileName = String

data OutputFormat
   = CrossReferences [FileName]
   | Data
   | LikelihoodModel
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
