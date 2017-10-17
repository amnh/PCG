-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation.Trans
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Evaluation' type's monad transformer definition and types.
--
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Evaluation.Trans where

import Control.Applicative
import Control.Evaluation.Internal
import Control.Evaluation.Unit
import Control.Monad (MonadPlus(mzero, mplus), join, liftM2)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Monoid   ()
import Data.Semigroup


-- |
-- A monad transformer of 'Evaluation'.
newtype EvaluationT m a
      = EvaluationT
      { -- | Run the 'EvaluationT' monad transformer
        runEvaluation :: m (Evaluation a)
      } 


-- | (✔)
instance Monad m => Alternative (EvaluationT m) where

    empty = mempty

    (<|>) x y = EvaluationT $ liftM2 (<|>) (runEvaluation x) (runEvaluation y)


-- | (✔)
instance Applicative m => Applicative (EvaluationT m) where

    pure = EvaluationT . pure . pure

    f <*> x = EvaluationT $ liftA2 (<*>) (runEvaluation f) (runEvaluation x)
  

-- | (✔)
instance Functor m => Functor (EvaluationT m) where

    fmap f x = EvaluationT . fmap (fmap f) $ runEvaluation x


-- | (✔)
instance Monad m => Logger (EvaluationT m) a where

    info = state . info

    warn = state . warn


-- | (✔)
instance Monad m => Monad (EvaluationT m) where

    return  = pure

    fail    = EvaluationT . pure . fail

    x >>  y = EvaluationT $ liftM2 (>>) (runEvaluation x) (runEvaluation y)
    x >>= f = EvaluationT $ do
                y <- runEvaluation x
                case y of
                  Evaluation ns  NoOp     -> pure . Evaluation ns $ NoOp
                  Evaluation ns (Error e) -> pure . Evaluation ns $ Error e
                  Evaluation ns (Value v) -> (`prependNotifications` ns) <$> runEvaluation (f v)


-- | (✔)
instance Monad m => MonadPlus (EvaluationT m) where

    mzero = mempty

    mplus = (<>)


-- | (✔)
instance MonadIO m => MonadIO (EvaluationT m) where

    liftIO = lift . liftIO


-- | (✔)
instance MonadTrans EvaluationT where

    lift = EvaluationT . fmap pure


-- | (✔)
instance Monad m => Monoid (EvaluationT m a) where

    mempty  = EvaluationT $ pure mempty

    mappend = (<>)


-- | (✔)
instance Monad m => Semigroup (EvaluationT m a) where

    x <> y = EvaluationT $ liftM2 (<>) (runEvaluation x) (runEvaluation y)


-- |
-- Takes an 'IO' value and lifts it into the evaluation context.
impure :: IO a -> EvaluationT IO a
impure = liftIO


-- |
-- Takes an 'Evaluation' and lifts it into the transformer context.
state :: Monad m => Evaluation a -> EvaluationT m a
state = EvaluationT . pure 


-- |
-- Prints an 'IO' parameterized transformer of 'Evaluation' context to
-- the STDOUT.
showRun :: Show a => EvaluationT IO a -> IO ()
showRun = join . fmap print . runEvaluation
