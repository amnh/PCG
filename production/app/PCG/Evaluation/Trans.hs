{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module PCG.Evaluation.Trans where

import Control.Applicative
import Control.Monad          (MonadPlus(mzero, mplus), join, liftM2)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid

import PCG.Evaluation.Internal
import PCG.Evaluation.Unit
import PCG.Graph

type SearchState = EvaluationT IO Graph

newtype EvaluationT m a
      = EvaluationT
      { runEvaluation :: m (Evaluation a)
      }

instance Functor m => Functor (EvaluationT m) where
  fmap f x = EvaluationT . fmap (fmap f) $ runEvaluation x

instance Applicative m => Applicative (EvaluationT m) where
  pure = EvaluationT . pure . pure
  f <*> x = EvaluationT $ liftA2 (<*>) (runEvaluation f) (runEvaluation x)
  
instance Monad m => Monad (EvaluationT m) where
  return  = pure
  fail    = EvaluationT . pure . fail
  x >>  y = EvaluationT $ liftM2 (>>) (runEvaluation x) (runEvaluation y)
  x >>= f = EvaluationT $ do
              y <- runEvaluation x
              case y of
                Evaluation ns  NoOp     -> pure . Evaluation ns $ NoOp
                Evaluation ns (Error e) -> pure . Evaluation ns $ Error e
                Evaluation ns (Value v) -> liftM2 prependNotifications (runEvaluation $ f v) (pure ns)

instance MonadIO m => MonadIO (EvaluationT m) where
  liftIO = lift . liftIO

instance Monad m => MonadPlus (EvaluationT m) where
    mzero = mempty
    mplus = (<>)

instance MonadTrans EvaluationT where
  lift = EvaluationT . fmap pure

instance Monad m => Monoid (EvaluationT m a) where
    mempty = EvaluationT $ pure mempty
    mappend x y = EvaluationT $ liftM2 (<>) (runEvaluation x) (runEvaluation y)

instance Monad m => Alternative (EvaluationT m) where
    empty = mempty
    (<|>) x y = EvaluationT $ liftM2 (<|>) (runEvaluation x)(runEvaluation y)

instance Monad m => Logger (EvaluationT m) a where
  info = state . info
  warn = state . warn
           
impure :: IO a -> EvaluationT IO a
impure = liftIO

state :: Monad m => Evaluation a -> EvaluationT m a
state = EvaluationT . pure 

showRun :: Show a => EvaluationT IO a -> IO ()
showRun = join . fmap print . runEvaluation
