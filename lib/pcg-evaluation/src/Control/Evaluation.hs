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
  , EvaluationResult()
  , Notification(..)
  , ErrorPhase(..)
  -- * Run evaluation
  , runEvaluation
  , runEvaluationT
  -- * Elimination function
  , evaluateResult
  -- * Evaluation constructors
  , evaluateEither
  , failWithPhase
  -- * Rendering
  , showRun
  ) where

import Control.Evaluation.Notification
import Control.Evaluation.Result
import Control.Evaluation.Trans
import Data.Text.Lazy


-- |
-- Lifts an 'Either' with a `Show` error condition into the 'Evaluation' context.
evaluateEither :: Show s => Either s b -> Evaluation r b
evaluateEither (Left  e) = fail $ show e
evaluateEither (Right x) = pure x


-- |
-- Elimination function for the 'Evaluation' type.
evaluateResult
  :: (ErrorPhase -> Text -> b) -- ^ How to consume the error message when an error has occured
  -> (a -> b)                  -- ^ How to transform the stored value
  -> EvaluationResult a
  -> b
evaluateResult err val x =
    case runEvaluationResult x of
      Left  (p,s) -> err p s
      Right v     -> val v
