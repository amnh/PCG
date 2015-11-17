{-# LANGUAGE DeriveFunctor #-}
module PCG.Evaluation
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

import PCG.Evaluation.Internal
import PCG.Evaluation.Trans
import PCG.Evaluation.Unit

evalIO :: IO a -> EvaluationT IO a
evalIO = impure

evalEither :: Show a => Either a b -> Evaluation b
evalEither (Left  e) = fail $ show e
evalEither (Right x) = pure x
