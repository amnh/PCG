module PCG.Computation.Internal where

import           Bio.Graph
import           Control.DeepSeq
import           Control.Evaluation
import           Data.Char          (isSpace)
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup
import qualified PCG.Command.Build.Evaluate  as Build
import qualified PCG.Command.Read.Evaluate   as Read
import qualified PCG.Command.Report.Evaluate as Report
import           PCG.Syntax

import Debug.Trace


optimizeComputation :: Computation -> Computation
optimizeComputation (Computation commands) = Computation $ collapseReadCommands commands


collapseReadCommands :: NonEmpty Command -> NonEmpty Command
collapseReadCommands p@(x:|xs) =
    case xs of
     []   -> p
     y:ys ->
       case (x, y) of
         (READ lhs, READ rhs) -> collapseReadCommands (READ (lhs<>rhs) :| ys)
         _ -> (x :|) . toList . collapseReadCommands $ y:|ys


evaluate :: Computation -> SearchState
evaluate (Computation xs) = foldl' f mempty xs
  where
    f :: SearchState -> Command ->  SearchState
    f state x@BUILD  {} = Build.evaluate  x (force state)
    f state x@READ   {} = Read.evaluate   x (force state)
    f state x@REPORT {} = Report.evaluate x (force state)
--    f _ = error "NOT YET IMPLEMENTED"


renderSearchState :: Evaluation a -> IO ()
renderSearchState e = do
   _ <- case notifications e of
          [] -> pure ()
          xs -> putStrLn . unlines $ show <$> xs
   case evaluationResult e of
     NoOp         -> putStrLn   "[❓] No computation speciified...?"
     Value _      -> putStrLn   "[✔] Computation complete!"
     Error errMsg -> putStrLn $ "[✘] Error: "<> trimR errMsg
  where
    trimR = reverse . dropWhile isSpace . reverse
