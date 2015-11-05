module PCG.Script.Types where

import Data.Int              (Int64)
import Data.Time.Clock       (DiffTime)

-- | DubiousCommands are "Stringly-Typed" and therefore inherently unsafe.
-- We will later consume a list of DubiousCommands as a Script type and
-- convert these into thier less dubious, well-type counterpart of type Command,
-- or report an error explaing why the DubiousCommand is not valid.
data DubiousCommand = DubiousCommand Lident [Argument] deriving (Show)

data Script    = Script [DubiousCommand]
               deriving (Show)
newtype Lident = Lident String
               deriving (Show)
data Argument  = PrimativeArg Primative
               | LidentArg Lident
               | LidentNamedArg Lident Argument
               | CommandArg DubiousCommand
               | ArgumentList [Argument]
               deriving (Show)
data Primative = WholeNum  Int64
               | RealNum   Double
               | BitValue  Bool
               | TextValue String
               | TimeSpan  DiffTime
               deriving (Show)
