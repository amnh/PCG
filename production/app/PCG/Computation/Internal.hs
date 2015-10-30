module PGC.Computation.Internal where

import Data.Char             (toLower)
import Data.Either           (partitionEithers)
import Data.Int              (Int64)
import Data.Map              (Map,fromList,lookup)
import Data.Maybe            (fromJust)
import Data.Time.Clock       (DiffTime,secondsToDiffTime)
import Text.Parsec           (Parsec)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Prelude hiding (lookup)

import PCG.Command
import PCG.Script

data Computation = Computation [Command]
  deriving (Show)

interpret :: Script -> Either [String] Computation
interpret (Script xs) =
  case partitionEithers $ rebukeDubiousness <$> xs of
    ([]    , actions) -> Right $ Computation actions
    (errors, _      ) -> Left errors
