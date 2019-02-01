{-# LANGUAGE FlexibleContexts #-}

module PCG.Computation.Internal
  ( evaluate
  , getGlobalSettings
  , optimizeComputation
  , renderSearchState
  ) where

import           Bio.Graph
import           Control.Evaluation
import           Data.Char                   (isSpace)
import           Data.Foldable
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified PCG.Command.Build.Evaluate  as Build
import qualified PCG.Command.Echo.Evaluate   as Echo
import qualified PCG.Command.Load.Evaluate   as Load
import qualified PCG.Command.Read.Evaluate   as Read
import qualified PCG.Command.Report.Evaluate as Report
import qualified PCG.Command.Save.Evaluate   as Save
import           PCG.Syntax
import           System.Exit


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
              BUILD  x -> v >>= Build.evaluate  x
              ECHO   x -> v >>= Echo.evaluate   x
              LOAD   x -> v >>= Load.evaluate   x
              READ   x -> v *>  Read.evaluate   x
              REPORT x -> v >>= Report.evaluate x
              SAVE   x -> v >>= Save.evaluate   x


renderSearchState :: Evaluation a -> (ExitCode, String)
renderSearchState = evaluation def err val
  where
    trimR = reverse . dropWhile isSpace . reverse

    def        = (ExitFailure 3, "[❓] No computation speciified...?")
    err errMsg = (ExitFailure 5, "[✘] Error: "<> trimR errMsg       )
    val _      = (ExitSuccess  , "[✔] Computation complete!"        )



getGlobalSettings :: IO GlobalSettings
getGlobalSettings = pure ()
