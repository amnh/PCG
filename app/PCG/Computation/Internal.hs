module PCG.Computation.Internal
  ( evaluate
  , optimizeComputation
  , renderSearchState
  ) where

import           Bio.Graph
import           Control.Evaluation
import           Data.Char                   (isSpace)
import           Data.Foldable
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified PCG.Command.Build.Evaluate  as Build
import qualified PCG.Command.Load.Evaluate   as Load
import qualified PCG.Command.Read.Evaluate   as Read
import qualified PCG.Command.Report.Evaluate as Report
import qualified PCG.Command.Save.Evaluate   as Save
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
         _                    -> (x :|) . toList . collapseReadCommands $ y:|ys


evaluate :: Computation -> SearchState
evaluate (Computation xs) = foldl' f mempty xs
  where
    f :: SearchState -> Command -> SearchState
    f v c = case c of
              BUILD  _ -> v >>= Build.evaluate  c
              READ   _ -> v *>  Read.evaluate   c
              REPORT _ -> v >>= Report.evaluate c
              SAVE   _ -> v >>= Save.evaluate   c
              LOAD   _ -> v >>= Load.evaluate   c


renderSearchState :: Evaluation a -> String
renderSearchState e =
   case evaluationResult e of
     NoOp         -> "[❓] No computation speciified...?"
     Value _      -> "[✔] Computation complete!"
     Error errMsg -> "[✘] Error: "<> trimR errMsg
  where
    trimR = reverse . dropWhile isSpace . reverse
