module PCG.Command.Types where

import Data.Char             (toLower)
import Data.Either           (partitionEithers)
import Data.Int              (Int64)
import Data.Map              (Map,fromList,lookup)
import Data.Maybe            (fromJust)
import Data.Time.Clock       (DiffTime,secondsToDiffTime)
import Prelude hiding (lookup)

data Command
  = READ [String]
  | ECHO String EchoClass
  | EXIT
  deriving (Show)

data EchoClass = Info | Warn | Path String deriving (Show)
