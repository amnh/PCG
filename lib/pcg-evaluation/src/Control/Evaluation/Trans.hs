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
import           Control.Monad.Fail          (MonadFail)
import qualified Control.Monad.Fail          as F
import           Control.Monad.Fix           (MonadFix (..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict (MonadWriter (..))
import           Control.Monad.Zip           (MonadZip (..))
import           Data.DList                  (DList)
import           Data.Functor.Alt            (Alt (..))
import           Data.Functor.Apply          (Apply (..))
import           Data.Functor.Bind           (Bind (..))
import           Data.Functor.Classes        (Eq1 (..), Ord1 (..), Show1 (..))
import           GHC.Generics
import           Test.QuickCheck


-- |
-- A monad transformer of 'Evaluation'.
newtype EvaluationT m a
      = EvaluationT
      { -- | Run the 'EvaluationT' monad transformer
        runEvaluation :: m (Evaluation a)
      } deriving (Generic)


instance Alt m => Alt (EvaluationT m) where

    {-# INLINEABLE (<!>) #-}

    (<!>) x y = EvaluationT $ runEvaluation x <!> runEvaluation y


instance Monad m => Applicative (EvaluationT m) where

    {-# INLINEABLE (<*>) #-}
    {-# INLINE     (*>)  #-}
    {-# INLINE     pure  #-}

    pure = state . pure

    (<*>) = apply

    (*>)  = propogate


instance Monad m => Apply (EvaluationT m) where

    {-# INLINEABLE (<.>) #-}
    {-# INLINE     (.>)  #-}

    (<.>) = apply

    (.>)  = propogate


instance (Arbitrary a, Arbitrary1 m) => Arbitrary (EvaluationT m a) where

    {-# INLINE arbitrary #-}

    arbitrary = EvaluationT <$> liftArbitrary arbitrary


instance (Apply m, Monad m) => Bind (EvaluationT m) where

    {-# INLINEABLE (>>-) #-}

    (>>-) = bind


instance Foldable m => Foldable (EvaluationT m) where

    {-# INLINEABLE foldMap #-}

    foldMap f = foldMap (foldMap f) . runEvaluation


instance (Eq a, Eq1 m) => Eq (EvaluationT m a) where

    {-# INLINE (==) #-}

    (==) x = liftEq (==) (runEvaluation x) . runEvaluation


instance Eq1 m => Eq1 (EvaluationT m) where

    {-# INLINE liftEq #-}

    liftEq f lhs = liftEq (liftEq f) (runEvaluation lhs) . runEvaluation


instance Functor m => Functor (EvaluationT m) where

    {-# INLINEABLE fmap #-}

    fmap f x = EvaluationT . fmap (fmap f) $ runEvaluation x


instance Monad m => Logger (EvaluationT m) a where

    {-# INLINE (<?>) #-}
    {-# INLINE (<@>) #-}

    x <?> s = EvaluationT $ (<?> s) <$> runEvaluation x

    x <@> s = EvaluationT $ (<@> s) <$> runEvaluation x


instance (Monad m, NFData a) => NFData (EvaluationT m a) where

    {-# INLINE rnf #-}

    rnf (EvaluationT x) = (force <$> x) `seq` ()


instance Monad m => Monad (EvaluationT m) where

    {-# INLINEABLE (>>=)  #-}
    {-# INLINE     (>>)   #-}
    {-# INLINE     return #-}
    {-# INLINE     fail   #-}

    (>>=)  = bind

    (>>)   = (*>)

    return = pure

    fail   = F.fail


instance Monad m => MonadFail (EvaluationT m) where

    {-# INLINE fail #-}

    fail = state . fail


instance Monad m => MonadFix (EvaluationT m) where

    mfix f = let a = a >>= f in a


instance MonadIO m => MonadIO (EvaluationT m) where

    {-# INLINE liftIO #-}

    liftIO = lift . liftIO


instance MonadReader r m => MonadReader r (EvaluationT m) where

    ask    = lift ask

    local  = mapEvaluationT . local

    reader = lift . reader


instance MonadTrans EvaluationT where

    {-# INLINE lift #-}

    lift = EvaluationT . fmap pure


instance Monad m => MonadWriter (DList Notification) (EvaluationT m) where

    writer = EvaluationT . pure . writer

    listen = EvaluationT . fmap listen . runEvaluation

    pass   = EvaluationT . fmap pass . runEvaluation


instance Monad m => MonadZip (EvaluationT m) where

    {-# INLINEABLE mzip     #-}
    {-# INLINEABLE munzip   #-}
    {-# INLINE     mzipWith #-}

    mzip     = liftA2 (,)

    mzipWith = liftA2

    munzip x = let !v = runEvaluation x in (f fst v, f snd v)
      where
        f t = EvaluationT . fmap (t . munzip)


instance (Ord1 m, Ord a) => Ord (EvaluationT m a) where

    {-# INLINE compare #-}

    compare = liftCompare compare


instance Ord1 m => Ord1 (EvaluationT m) where

    {-# INLINE liftCompare #-}

    liftCompare cmp lhs = liftCompare (liftCompare cmp) (runEvaluation lhs) . runEvaluation


instance Applicative m => Semigroup (EvaluationT m a) where

    {-# INLINE (<>) #-}

    x <> y = EvaluationT $ liftA2 (<>) (runEvaluation x) (runEvaluation y)


instance (Show a, Show1 m) => Show (EvaluationT m a) where

    showsPrec n = liftShowsPrec showsPrec showList n . runEvaluation


instance (Traversable m) => Traversable (EvaluationT m) where

    traverse f = fmap EvaluationT . traverse (traverse f) . runEvaluation


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


bind :: Monad m => EvaluationT m a -> (a -> EvaluationT m b) -> EvaluationT m b
bind x f = EvaluationT $ do
    (Evaluation ns y) <- runEvaluation x           -- :: Evaluation a
    case runEvalUnit y of
      Left  s -> pure . Evaluation ns . EU $ Left s
      Right v -> (`prependNotifications` ns) <$> runEvaluation (f v)


apply :: Monad m => EvaluationT m (t -> a) -> EvaluationT m t -> EvaluationT m a
apply lhs rhs = EvaluationT $ do
    (Evaluation ms x) <- runEvaluation lhs
    case runEvalUnit x of
      Left  s -> pure . Evaluation ms . EU $ Left s
      Right f -> do
          (Evaluation ns y) <- runEvaluation rhs
          pure . Evaluation (ms <> ns) . EU $ case runEvalUnit y of
                                                Left  s -> Left s
                                                Right v -> Right $ f v


propogate :: Monad m => EvaluationT m a -> EvaluationT m b -> EvaluationT m b
propogate lhs rhs = EvaluationT $ do
    (Evaluation ms x) <- runEvaluation lhs
    case runEvalUnit x of
      Left  s -> pure . Evaluation ms . EU $ Left s
      Right _ -> do
          (Evaluation ns y) <- runEvaluation rhs
          pure $ Evaluation (ms <> ns) y
