module PCG.Computation.Internal where

import Bio.PhyloGraph.Solution
import Control.Evaluation
import Data.Char      (isSpace)
import Data.Either    (partitionEithers)
import Data.Monoid
import PCG.Command
import PCG.Script

import qualified PCG.Command.Types.Read   as Read
import qualified PCG.Command.Types.Report as Report

data Computation = Computation [Command]
  deriving (Show)

interpret :: Script -> Either [String] Computation
interpret (Script xs) =
  case partitionEithers $ rebukeDubiousness <$> xs of
    ([]    , actions) -> Right . optimizeComputation $ Computation actions
    (errors, _      ) -> Left errors

optimizeComputation :: Computation -> Computation
optimizeComputation (Computation commands) = Computation $ collapseReadCommands commands

collapseReadCommands :: [Command] -> [Command]
collapseReadCommands []                       = []
collapseReadCommands (READ x1 : READ x2 : xs) = collapseReadCommands (READ (x1<>x2) : xs)
collapseReadCommands (x:xs)                   = x : collapseReadCommands xs
    
evaluate :: Computation -> SearchState
evaluate (Computation xs) = foldl (flip f) (pure mempty) xs
  
f :: Command -> SearchState -> SearchState
f x@READ   {} = Read.evaluate   x
f x@REPORT {} = Report.evaluate x
f _ = error "NOT YET IMPLEMENTED"

renderSearchState :: (Show a) => Evaluation a -> IO ()
renderSearchState e = do
   _ <- case notifications e of
          [] -> pure ()
          xs -> putStrLn . unlines $ show <$> xs
   case evaluationResult e of
     NoOp         -> putStrLn   "[?] No computation speciified...?"
     Value _      -> putStrLn   "[✔] Computation complete!"
     Error errMsg -> putStrLn $ "[X] Error: "<> trimR errMsg
  where
    trimR = reverse . dropWhile isSpace . reverse
