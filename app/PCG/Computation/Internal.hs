{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

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
evaluate (Computation (x:|xs)) =
    case x of
      READ c -> foldl' f (Read.evaluate c) xs
      LOAD c -> foldl' f (Load.evaluate c) xs
      _      -> fail "There was no input data specified to start the computation; expecting a LOAD or READ command."
  where
    f :: SearchState -> Command -> SearchState
    f s = \case
             BUILD  c -> s >>=  Build.evaluate c
             ECHO   c -> s >>=   Echo.evaluate c
             LOAD   c -> s *>    Load.evaluate c
             READ   c -> s *>    Read.evaluate c
             REPORT c -> s >>= Report.evaluate c
             SAVE   c -> s >>=   Save.evaluate c


renderSearchState :: Evaluation a -> (ExitCode, String)
renderSearchState eval = (unlines renderedNotifications <>) <$> evaluation err val eval
  where
    renderedNotifications = f <$> notifications eval
      where
        f :: Notification -> String
        f (Information s) = "[-] " <> toList s
        f (Warning     s) = "[!] " <> toList s

    trimR = reverse . dropWhile isSpace . reverse

    err errMsg = (ExitFailure 5, "[✘] Error: "<> trimR errMsg       )
    val _      = (ExitSuccess  , "[✔] Computation complete!"        )


getGlobalSettings :: IO GlobalSettings
getGlobalSettings = pure ()
