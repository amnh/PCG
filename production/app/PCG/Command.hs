module PCG.Command
  (-- rebukeDubiousness
  ) where

import Data.Char      (toLower)
import Data.Maybe     (fromMaybe)
import Data.Map       (Map,fromList,lookup)
import Prelude hiding (lookup)

--import PCG.Command.Types
import PCG.Syntax

import qualified PCG.Command.Types.Read   as Read
import qualified PCG.Command.Types.Report as Report

--rebukeDubiousness :: DubiousCommand -> Either String Command
--rebukeDubiousness dubious = fromMaybe (Left $ "Command not found: " ++ show dubious) $ commandInterpretation dubious

--commandInterpretation :: DubiousCommand -> Maybe (Either String Command)
--commandInterpretation (DubiousCommand (Lident name) args) = (toLower <$> name) `lookup` validCommands <*> pure args

{-
validCommands :: Map String ([Argument] -> Either String Command)
validCommands = fromList
  [ ("read"  , Read.validate)
  , ("report", Report.validate)
  ]
-}

--commandEvaluations :: Command -> Evaluation IO ()
