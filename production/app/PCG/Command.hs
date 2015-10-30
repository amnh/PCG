module PCG.Command
  ( module PCG.Command.Types
  , rebukeDubiousness
  ) where

import Data.Char             (toLower)
import Data.Either           (partitionEithers)
import Data.Int              (Int64)
import Data.Map              (Map,fromList,lookup)
import Data.Maybe            (fromJust)
import Data.Time.Clock       (DiffTime,secondsToDiffTime)
import Prelude hiding (lookup)

import PCG.Command.Types
import PCG.Script.Types

import qualified PCG.Command.Types.Read as Read

rebukeDubiousness :: DubiousCommand -> Either String Command
rebukeDubiousness x =
  case commandInterpretation x of
    Just x  -> x
    Nothing -> Left $ "Command not found: " ++ show x

commandInterpretation :: DubiousCommand -> Maybe (Either String Command)
commandInterpretation (DubiousCommand (Lident name) args) = (toLower <$> name) `lookup` validCommands <*> pure args

validCommands :: Map String ([Argument] -> Either String Command)
validCommands = fromList
  [ ("read", Read.validate)
  ]
  
--commandEvaluations :: Command -> Evaluation IO ()
