------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.Evaluate
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Read.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Evaluation
import Control.Monad                             (when)
import Control.Monad.IO.Class
import Control.Parallel.Custom
import Control.Parallel.Strategies
import Data.Semigroup.Foldable
import Data.Unification
import Data.Validation
import PCG.Command.Read
import PCG.Command.Read.DecorationInitialization
import PCG.Command.Read.InputStreams
import PCG.Command.Read.ParseStreams
import PCG.Command.Read.ReadCommandError


-- |
-- Evaluate a 'ReadCommand' to locate, parse, interpret, and transform file
-- contents into a working state for the process. Includes sophisticated error
-- handling and reporting to improve user experience.
evaluate :: ReadCommand -> SearchState
evaluate (ReadCommand fileSpecs) = do
    when (null fileSpecs) $ fail "No files specified in 'read()' command"
    parseResult <- liftIO . runValidationT . sequenceA $ parmap rpar (fmap removeGaps . parseSpecifiedFile) fileSpecs
    case parseResult of
      Failure pErr ->
        let phase = case pErr of
                      InputError {} -> Inputing
                      ParseError {} ->  Parsing
                      UnifyError {} -> Unifying
        in  failWithPhase phase pErr
      Success result ->
        case decoration . unifyPartialInputs $ transformation <$> fold1 result of
          Failure uErr -> failWithPhase Unifying uErr   -- Report structural errors here.
          -- TODO: rectify against 'old' SearchState, don't just blindly merge or ignore old state
          Success g    -> pure g
                         -- liftIO (putStrLn "DECORATION CALL:" *> print g) *> pure g
                         -- (liftIO . putStrLn {- . take 500000 -} $ either show (ppTopElement . toXML) g)
                         -- (liftIO . putStrLn $ show g) $> g
  where
    transformation = id -- expandIUPAC

    decoration
      :: Validation UnificationError (Either TopologicalResult CharacterResult)
      -> Validation UnificationError
           (Either
              TopologicalResult (PhylogeneticSolution FinalDecorationDAG))
    decoration     = fmapCharDAG initializeDecorations2

    fmapCharDAG = fmap . fmap


