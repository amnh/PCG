module PCG.Command.Types where

import PCG.Command.Types.Read.Internal
import PCG.Command.Types.Report.Internal

data Command
  = READ [FileSpecification]
  | REPORT OutputTarget OutputFormat
  | ECHO String EchoClass
  | ANALYZE String
  | EXIT
  deriving (Show)

data EchoClass     = Info | Warn | Path String deriving (Show)
