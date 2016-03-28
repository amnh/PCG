module PCG.Computation.Internal where

import Control.Evaluation
import Data.Either    (partitionEithers)
import PCG.Command
import PCG.Script

import qualified PCG.Command.Types.Read   as Read
import qualified PCG.Command.Types.Report as Report

data Computation = Computation [Command]
  deriving (Show)

interpret :: Script -> Either [String] Computation
interpret (Script xs) =
  case partitionEithers $ rebukeDubiousness <$> xs of
    ([]    , actions) -> Right $ Computation actions
    (errors, _      ) -> Left errors

evaluate :: Computation -> SearchState
evaluate (Computation []    ) = pure mempty
evaluate (Computation (x:xs)) = f x $ evaluate (Computation xs)

  
f :: Command -> SearchState -> SearchState
f x@READ   {} = Read.evaluate   x
f x@REPORT {} = Report.evaluate x
f _ = error "NOT YET IMPLEMENTED"
