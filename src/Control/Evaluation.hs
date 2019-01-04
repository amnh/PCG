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
  , Notification()
  , evalEither
  , evalIO
  , evaluation
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


evaluation
  :: b             -- ^ Default value when no computation has been performed
  -> (String -> b) -- ^ How to consume the error message when an error has occured
  -> (a -> b)      -- ^ How to transform the stored value
  -> Evaluation a -> b
evaluation def err val x =
  case evaluationResult x of
    NoOp    -> def
    Error s -> err s
    Value v -> val v
