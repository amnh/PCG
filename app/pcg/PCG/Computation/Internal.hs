-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Computation.Internal
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Defined how to evaluate a sequence of 'Command's.
--
-- Includes smart handling of certain signals with a "clean-up" routine which
-- writes out the current working state before exiting.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PCG.Computation.Internal
  ( evaluate
  , getGlobalSettings
  , optimizeComputation
  , renderSearchState
  ) where

import           Bio.Graph
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Evaluation
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Char                    (isSpace)
import           Data.FileSource
import           Data.Foldable
import           Data.List.NonEmpty           (NonEmpty(..))
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.IO            as T
import qualified Data.Text.Short              as ST
import           GHC.IO.Handle
import qualified PCG.Command.Build.Evaluate   as Build
import qualified PCG.Command.Echo.Evaluate    as Echo
import qualified PCG.Command.Load.Evaluate    as Load
import qualified PCG.Command.Read.Evaluate    as Read
import qualified PCG.Command.Report.Evaluate  as Report
import           PCG.Command.Save
import qualified PCG.Command.Save.Evaluate    as Save
import qualified PCG.Command.Version.Evaluate as Version
import           PCG.Syntax
import           System.Exit
import           System.IO
import           System.Posix.Signals


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
evaluate (Computation (x:|xs)) = liftIO myThreadId >>= \t -> foldl' (f t) z xs
  where
    z = case x of
          READ c -> Read.evaluate c
          LOAD c -> Load.evaluate c
          _      -> fail $ unwords
                      [ "There was no input data specified to start the computation;"
                      , "expecting a LOAD or READ command."
                      ]

    f :: ThreadId -> SearchState -> Command -> SearchState
    f t s =
        let g op a = cleanUpHandler t a *> op a
        in  \case
              BUILD   c -> s >>= g (      Build.evaluate c)
              ECHO    c -> s >>= g (       Echo.evaluate c)
              LOAD    c -> s >>= g (const (Load.evaluate c))
              READ    c -> s >>= g (const (Read.evaluate c))
              REPORT  c -> s >>= g (     Report.evaluate c)
              SAVE    c -> s >>= g (       Save.evaluate c)
              VERSION c -> s >>= g (    Version.evaluate c)


cleanUpHandler :: ThreadId -> GraphState -> EvaluationT GlobalSettings IO ()
cleanUpHandler tID lastState = liftIO $ traverse_ buildHandler [sigINT, sigQUIT, sigTERM]
  where
    buildHandler s =
      let err p t  = (Just t, errorPhaseToCode p)
          val      = const (Nothing, ExitSuccess)
          success  = putStrLn $ "Work state saved to: " <> ST.toString (toShortText defaultSaveFilePath)
          saveData = SaveCommand defaultSaveFilePath Binary
          handler  = CatchOnce . liftIO $ do
            hSetEncoding stdout utf8
            hSetEncoding stderr utf8
            hSetBuffering stdout NoBuffering
            hSetBuffering stderr NoBuffering
            putStr "\r\027[K"
            putStr "Saving current work state..."
            (msg,res) <- fmap force . runEvaluationT () . Save.evaluate saveData $ force lastState
            let (txt,code) = evaluateResult err val res
            putStr "\r\027[K"
            traverse_ (T.putStrLn . renderNotification) msg
            maybe success T.putStrLn txt
            hClose stdout
            hClose stderr
            liftIO $ throwTo tID code
            exitWith code
      in  installHandler s handler Nothing


renderSearchState :: EvaluationResult a -> (ExitCode, Text)
renderSearchState = fmap (<>"\n") . either id val . renderError
  where
    val _ = (ExitSuccess, "[✔] Computation complete!")


renderError :: EvaluationResult a -> Either (ExitCode, Text) a
renderError = evaluateResult (\p -> Left . err p) Right
  where
    err errPhase errMsg =
      (errorPhaseToCode errPhase, "[✘] Error: "<> trimR errMsg)

    trimR = T.dropWhileEnd isSpace


-- |
-- Get the error code associated with the phase in which the error occurred.
--
-- The error code will have one or more bits set in the range [2, 5].
-- The bits are set progressively as successive phases are passed.
--
-- If a failure occurred in the first phase, reading data from the file system,
-- then the first bit (index 2) in the range will be set.
--
-- If the failure occurred in the second phase, after data was successfully read
-- from the disk but could not be successfully parsed, then the first and second
-- bits (indices 2 & 3) in the range will be set.
--
-- If the failure occurred in the third phase, after data was read from the disk
-- and the streams were successfully parsed but the collection of data was not
-- consistent and could not be unified, then the first through third bits
-- (indices 2, 3 & 4) in the range will be set.
--
-- If the failure occurred after reading data from disk, parsing the data streams,
-- and unifying the input data, but an error occurred during the phylogenetic
-- search, then the first through fourth bits (indices 2, 3, 4 & 5) in the range
-- will be set.
errorPhaseToCode :: ErrorPhase -> ExitCode
errorPhaseToCode = ExitFailure .
    \case
      Inputing   -> bit 2
      Parsing    -> bit 3
      Unifying   -> bit 2 .|. bit 3
      Computing  -> bit 4
      Outputting -> bit 5


getGlobalSettings :: IO GlobalSettings
getGlobalSettings = pure ()
