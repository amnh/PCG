{-# LANGUAGE DeriveFunctor #-}
module PCG.Computation.Trans where

import Control.Applicative
import Control.Monad             (MonadPlus(mzero, mplus), liftM2, ap)
import Control.Monad.Fix         (MonadFix(mfix))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except (ExceptT(..))
import Data.Functor.Classes
import Data.Monoid

import PCG.Computation.Internal
import PCG.Computation.Unit

newtype ComputationT m a
      = ComputationT
      { runComputation :: m (Computation a)
      }

instance Functor m => Functor (ComputationT m) where
  fmap f x = ComputationT . fmap (fmap f) $ runComputation x

instance Applicative m => Applicative (ComputationT m) where
  pure = ComputationT . pure . pure
  f <*> x = ComputationT $ liftA2 (<*>) (runComputation f) (runComputation x)
  
instance Monad m => Monad (ComputationT m) where
  return  = pure
  fail    = ComputationT . pure . fail
  x >>  y = ComputationT $ liftM2 (>>) (runComputation x) (runComputation y)
  x >>= f = ComputationT $ do
              v <- runComputation x
              case v of
                Computation ns  NoOp     -> pure . Computation ns $ NoOp
                Computation ns (Error x) -> pure . Computation ns $ Error x
                Computation ns (Value x) -> liftM2 prependNotifications (runComputation $ f x) (pure ns)

instance MonadIO m => MonadIO (ComputationT m) where
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
instance Monad m => MonadPlus (ComputationT m) where
    mzero = mempty
    mplus = (<>)

instance MonadTrans ComputationT where
  lift = ComputationT . fmap pure

instance Monad m => Monoid (ComputationT m a) where
    mempty = ComputationT $ pure mempty
    mappend x y = ComputationT $ liftM2 (<>) (runComputation x) (runComputation y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Monad m => Alternative (ComputationT m) where
    empty = mempty
    (<|>) x y = ComputationT $ liftM2 (<|>) (runComputation x)(runComputation y)

impure :: IO a -> ComputationT IO a
impure = liftIO
