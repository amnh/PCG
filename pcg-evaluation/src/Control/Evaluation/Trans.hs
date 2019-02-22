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

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- This is needed due to the functional dependency in MonadReader.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Evaluation.Trans where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Evaluation.Internal
import           Control.Evaluation.Unit
import           Control.Monad               (MonadPlus (..))
import           Control.Monad.Fail          (MonadFail)
import qualified Control.Monad.Fail          as F
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict (MonadWriter (..))
import           Data.DList                  (DList)
import           GHC.Generics


-- |
-- A monad transformer of 'Evaluation'.
newtype EvaluationT m a
      = EvaluationT
      { -- | Run the 'EvaluationT' monad transformer
        runEvaluation :: m (Evaluation a)
      } deriving (Generic)


-- | (✔)
instance Applicative m => Alternative (EvaluationT m) where

    empty = mempty

    (<|>) x y = EvaluationT $ liftA2 (<|>) (runEvaluation x) (runEvaluation y)


-- | (✔)
instance Applicative m => Applicative (EvaluationT m) where

    pure = state . pure

    f <*> x = EvaluationT $ liftA2 (<*>) (runEvaluation f) (runEvaluation x)

    x  *> y = EvaluationT $ liftA2  (*>) (runEvaluation x) (runEvaluation y)


-- | (✔)
instance Functor m => Functor (EvaluationT m) where

    fmap f x = EvaluationT . fmap (fmap f) $ runEvaluation x


-- | (✔)
instance Monad m => Logger (EvaluationT m) a where

    info = state . info

    warn = state . warn


-- | (✔)
instance (Monad m, NFData a) => NFData (EvaluationT m a) where

    rnf (EvaluationT x) = (force <$> x) `seq` ()


-- | (✔)
instance Monad m => Monad (EvaluationT m) where

    fail    = F.fail

    return  = pure

    x >>= f = EvaluationT $ do
                !y <- runEvaluation x
                case y of
                  Evaluation ns  NoOp      -> pure . Evaluation ns $ NoOp
                  Evaluation ns (Error  e) -> pure . Evaluation ns $ Error e
                  Evaluation ns (Value !v) -> (`prependNotifications` ns) <$> runEvaluation (f v)


-- | (✔)
instance Monad m => MonadFail (EvaluationT m) where

    fail = state . fail


-- | (✔)
instance Monad m => MonadPlus (EvaluationT m) where

    mzero = mempty

    mplus = (<>)


-- | (✔)
instance MonadIO m => MonadIO (EvaluationT m) where

    liftIO = lift . liftIO


-- | (✔)
instance MonadReader r m => MonadReader r (EvaluationT m) where

    ask    = lift ask
    local  = mapEvaluationT . local
    reader = lift . reader


-- | (✔)
instance MonadTrans EvaluationT where

    lift = EvaluationT . fmap pure


-- | (✔)
instance Monad m => MonadWriter (DList Notification) (EvaluationT m) where

    writer = EvaluationT . pure . writer
    listen = EvaluationT . fmap listen . runEvaluation
    pass   = EvaluationT . fmap pass . runEvaluation


-- | (✔)
instance Applicative m => Monoid (EvaluationT m a) where

    mempty  = state mempty

    mappend = (<>)


-- | (✔)
instance Applicative m => Semigroup (EvaluationT m a) where

    x <> y = EvaluationT $ liftA2 (<>) (runEvaluation x) (runEvaluation y)


-- |
-- Takes an 'IO' value and lifts it into the evaluation context.
impure :: IO a -> EvaluationT IO a
impure = liftIO


-- |
-- Takes an 'Evaluation' and lifts it into the transformer context.
{-# INLINE state #-}
state :: Applicative m => Evaluation a -> EvaluationT m a
state = EvaluationT . pure


-- |
-- Prints an 'IO' parameterized transformer of 'Evaluation' context to
-- the STDOUT.
showRun :: Show a => EvaluationT IO a -> IO ()
showRun = (print =<<) . runEvaluation


-- |
-- Map between two `EvaluationT` computations.
mapEvaluationT :: (m (Evaluation a) -> n (Evaluation b)) -> EvaluationT m a -> EvaluationT n b
mapEvaluationT f = EvaluationT . f . runEvaluation
