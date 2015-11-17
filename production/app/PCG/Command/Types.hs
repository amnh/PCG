module PCG.Command.Types where

data Command
  = READ [String]
  | ECHO String EchoClass
  | EXIT
  deriving (Show)

data EchoClass = Info | Warn | Path String deriving (Show)
