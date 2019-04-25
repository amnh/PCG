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
  ( EvaluationT(..)
  , Evaluation()
  , Notification(..)
  , ErrorPhase(..)
  , evalEither
  , evalIO
  , evaluation
  , failWithPhase
  , impure
  , notifications
  , runEvaluation
  , state
  , showRun
  ) where

import Control.Evaluation.Internal
import Control.Evaluation.Trans
import Control.Evaluation.Unit
import Data.Text.Lazy


-- |
-- Synonym for 'impure'
evalIO :: IO a -> EvaluationT IO a
evalIO = impure


-- |
-- Lifts an 'Either' with a `Show` error condition into the 'Evaluation' context.
evalEither :: Show s => Either s b -> Evaluation b
evalEither (Left  e) = fail $ show e
evalEither (Right x) = pure x


-- |
-- Elimination function for the 'Evaluation' type.
evaluation
  :: (ErrorPhase -> Text -> b) -- ^ How to consume the error message when an error has occured
  -> (a -> b)                  -- ^ How to transform the stored value
  -> Evaluation a
  -> b
evaluation err val (Evaluation _ x) =
    case runEvalUnit x of
      Left  (p,s) -> err p s
      Right v     -> val v
