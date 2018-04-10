module PCG.Computation.Internal where

import           Bio.Graph
import           Control.Evaluation
import           Data.Char          (isSpace)
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup
import qualified PCG.Command.Build.Evaluate  as Build
import qualified PCG.Command.Read.Evaluate   as Read
import qualified PCG.Command.Report.Evaluate as Report
import           PCG.Syntax


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
    f :: SearchState -> Command -> SearchState
    f v c@BUILD  {} = v >>= Build.evaluate  c
    f v c@READ   {} = v *>  Read.evaluate   c
    f v c@REPORT {} = v >>= Report.evaluate c
--    f _ = error "NOT YET IMPLEMENTED"


renderSearchState :: Evaluation a -> String
renderSearchState e =
   case evaluationResult e of
     NoOp         -> "[❓] No computation speciified...?"
     Value _      -> "[✔] Computation complete!"
     Error errMsg -> "[✘] Error: "<> trimR errMsg
  where
    trimR = reverse . dropWhile isSpace . reverse
