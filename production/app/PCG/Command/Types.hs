module PCG.Command.Types where

import PCG.Command.Types.Read.Internal

data Command
  = READ [FileSpecification]
  | ECHO String EchoClass
  | ANALYZE String
  | EXIT
  deriving (Show)

data EchoClass     = Info | Warn | Path String deriving (Show)
