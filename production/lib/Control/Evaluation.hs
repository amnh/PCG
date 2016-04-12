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
  , EvalUnit(..)
  , SearchState
  , evalEither
  , evalIO
  , evaluationResult
  , notifications
  , runEvaluation
  , state
  , showRun
  ) where

import Control.Evaluation.Internal
import Control.Evaluation.Trans
import Control.Evaluation.Unit

evalIO :: IO a -> EvaluationT IO a
evalIO = impure

evalEither :: Show a => Either a b -> Evaluation b
evalEither (Left  e) = fail $ show e
evalEither (Right x) = pure x
