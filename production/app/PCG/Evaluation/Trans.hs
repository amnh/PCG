{-# LANGUAGE DeriveFunctor #-}
module PCG.Evaluation.Trans where

import Control.Applicative
import Control.Monad             (MonadPlus(mzero, mplus), liftM2, ap)
import Control.Monad.Fix         (MonadFix(mfix))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except (ExceptT(..))
import Data.Functor.Classes
import Data.Monoid

import PCG.Evaluation.Internal
import PCG.Evaluation.Unit

type ImpureEvaluation a = EvaluationT IO a
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
              v <- runEvaluation x
              case v of
                Evaluation ns  NoOp     -> pure . Evaluation ns $ NoOp
                Evaluation ns (Error x) -> pure . Evaluation ns $ Error x
                Evaluation ns (Value x) -> liftM2 prependNotifications (runEvaluation $ f x) (pure ns)

instance MonadIO m => MonadIO (EvaluationT m) where
  liftIO = lift . liftIO

-- | Satisifying:
-- * Monoid
--     mplus mzero a = a
--     mplus a mzero = a
--     mplus (mplus a b) c = mplus a (mplus b c)
-- * Left Zero
--     mzero >>= k = mzero
-- * Left Distribution
--     mplus a b >>= k = mplus (a >>= k) (b >>= k)
instance Monad m => MonadPlus (EvaluationT m) where
    mzero = mempty
    mplus = (<>)

instance MonadTrans EvaluationT where
  lift = EvaluationT . fmap pure

instance Monad m => Monoid (EvaluationT m a) where
    mempty = EvaluationT $ pure mempty
    mappend x y = EvaluationT $ liftM2 (<>) (runEvaluation x) (runEvaluation y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Monad m => Alternative (EvaluationT m) where
    empty = mempty
    (<|>) x y = EvaluationT $ liftM2 (<|>) (runEvaluation x)(runEvaluation y)

impure :: IO a -> ImpureEvaluation a
impure = liftIO

trans :: Evaluation a -> EvaluationT IO a
trans = EvaluationT . pure 
