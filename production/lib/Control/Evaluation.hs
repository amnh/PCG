{-# LANGUAGE DeriveFunctor #-}
module Control.Evaluation
  ( EvaluationT()
  , Evaluation()
  , EvalUnit()
  , SearchState
  , Alternative(..)
  , Monoid(..)
  , (<>)
  , (<!>)
  , (<?>)
  , evalEither
  , evalIO
  , info
  , notifications
  , runEvaluation
  , state
  , showRun
  , warn
  ) where

import Control.Applicative (Alternative(..))
import Data.Monoid         ((<>))

import Control.Evaluation.Internal
import Control.Evaluation.Trans
import Control.Evaluation.Unit

evalIO :: IO a -> EvaluationT IO a
evalIO = impure

evalEither :: Show a => Either a b -> Evaluation b
evalEither (Left  e) = fail $ show e
evalEither (Right x) = pure x
