-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Evaluation type for representing a computational context.
--
-----------------------------------------------------------------------------

module Control.Evaluation
  ( EvaluationT()
  , Evaluation()
  , EvalUnit(..) -- TODO: Restructure so we don’t export this internal structure!
  , Notification()
  , evalEither
  , evalIO
  , evaluationResult
  , impure
  , notifications
  , runEvaluation
  , state
  , showRun
  ) where

import Control.Evaluation.Internal
import Control.Evaluation.Trans
import Control.Evaluation.Unit


-- |
-- Synonym for 'impure'
evalIO :: IO a -> EvaluationT IO a
evalIO = impure


-- |
-- Lifts an 'Either' with a `Show` error condition into the 'Evaluation' context.
evalEither :: Show s => Either s b -> Evaluation b
evalEither (Left  e) = fail $ show e
evalEither (Right x) = pure x
