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
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- This is needed due to the functional dependency in MonadReader.
{-# LANGUAGE UndecidableInstances  #-}

module Control.Evaluation.Trans
  ( Evaluation()
  , EvaluationT()
  -- * Run computation
  , runEvaluation
  , runEvaluationT
  -- * Other
  , failWithPhase
  , showRun
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Evaluation.Notification
import           Control.Evaluation.Result
import           Control.Monad.Fix               (MonadFix (..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS.CPS     (RWST, evalRWST, runRWST, rwsT)
import qualified Control.Monad.Trans.RWS.CPS     as RWS
import           Control.Monad.Zip               (MonadZip (..))
import           Data.Functor.Alt                (Alt (..))
import           Data.Functor.Apply              (Apply (..))
import           Data.Functor.Bind               (Bind (..))
import           Data.Functor.Identity
import           Data.Sequence                   (Seq, fromList)
import           Data.String
import           Data.Tuple                      (swap)
import           GHC.Generics
import           Test.QuickCheck
import           TextShow                        (TextShow)


-- |
-- A computational "evaluation."
--
-- An evaluation has global state @r@, accessible to it's computation.
--
-- An evaluation can be in one of two states, "successful" or "failure".
-- Use 'pure' to place a value inside a successful computational context.
-- Use 'fail' to indicate a computational failure.
--
-- A computational evaluation short-circuits at the first failure encountered.
-- The semigroup operator '(<>)' reflects this.
-- The alternative operator '(<!>)' inverts this logic, short-circuiting at the first sucess.
-- The following should hold:
--
-- > foldr1 (<>)  [fail x, fail y, pure z] === fail x
-- > foldr1 (<!>) [fail x, fail y, pure z] === pure z
--
-- A computation also stores an ordered log of 'Notification's.
-- Use the information operator '(<?>)' and the warning operator '(<@>)' to log computational notes.
--
-- Use 'runEvaluationT' to get the result of the computation.
type Evaluation r a = EvaluationT r Identity a


-- |
-- A computational "evaluation" monad transformer.
--
newtype EvaluationT r m a
      = EvaluationT
      { -- | Run the 'EvaluationT' monad transformer
        unwrapEvaluationT :: RWST r (Seq Notification) () m (EvaluationResult a)
      } deriving stock (Generic)


-- |
-- Exists to assist in defining the 'Arbitrary' instance for 'EvaluationT'.
newtype EvalHelper m a
      = EvalHelper { runEvalHelper :: m (EvaluationResult a, Seq Notification) }


instance Monad m => Alt (EvaluationT r m) where

    {-# INLINEABLE (<!>) #-}

    (<!>) x y = EvaluationT $ rwsT f
      where
        f r s =
          let v = runRWST (unwrapEvaluationT x) r s
          in do (e,_,_) <- v
                case runEvaluationResult e of
                  Left  _ -> runRWST (unwrapEvaluationT y) r s
                  Right _ -> v


instance Monad m => Applicative (EvaluationT r m) where

    {-# INLINEABLE (<*>) #-}
    {-# INLINE     (*>)  #-}
    {-# INLINE     pure  #-}

    pure = EvaluationT . pure . pure

    (<*>) = apply

    (*>)  = propogate


instance Monad m => Apply (EvaluationT r m) where

    {-# INLINEABLE (<.>) #-}
    {-# INLINE     (.>)  #-}

    (<.>) = apply

    (.>)  = propogate


instance (Arbitrary a, Arbitrary1 m) => Arbitrary (EvalHelper m a) where

    {-# INLINEABLE arbitrary #-}

    arbitrary = do
      notificationList <- fromList <$> arbitrary
      evalUnit         <- arbitrary
      let pairArb = pure (evalUnit, notificationList)
      EvalHelper <$> liftArbitrary pairArb


instance (Arbitrary a, Arbitrary1 m, CoArbitrary r, Function r, Functor m) => Arbitrary (EvaluationT r m a) where

    {-# INLINEABLE arbitrary #-}

    arbitrary = do
      rToEvalHelper <- applyFun <$> arbitrary
      pure . EvaluationT . rwsT . handleState $ runEvalHelper <$> rToEvalHelper

      where
        handleState :: (r -> m (EvaluationResult a, w)) -> (r -> () -> m (EvaluationResult a, (), w))
        handleState rmaw r _ = f <$> rmaw r

        f (a,w) = (a, (), w)


instance (Apply m, Monad m) => Bind (EvaluationT r m) where

    {-# INLINEABLE (>>-) #-}

    (>>-) = bind


instance Functor m => Functor (EvaluationT r m) where

    {-# INLINEABLE fmap #-}

    fmap f x = EvaluationT . fmap (fmap f) $ unwrapEvaluationT x


instance Monad m => Logger (EvaluationT r m) a where

    {-# INLINE (<?>) #-}
    {-# INLINE (<@>) #-}

    (<?>) = appendNote Information

    (<@>) = appendNote Warning


instance (Monad m, NFData a) => NFData (EvaluationT r m a) where

    {-# INLINE rnf #-}

    rnf (EvaluationT x) = (rnf <$> x) `seq` ()


instance Monad m => Monad (EvaluationT r m) where

    {-# INLINEABLE (>>=)  #-}
    {-# INLINE     (>>)   #-}
    {-# INLINE     return #-}

    (>>=)  = bind

    (>>)   = (*>)

    return = pure


instance Monad m => MonadFail (EvaluationT r m) where

    {-# INLINE fail #-}

    fail = EvaluationT . pure . fail


instance Monad m => MonadFix (EvaluationT r m) where

    mfix f = let a = a >>= f in a


instance MonadIO m => MonadIO (EvaluationT r m) where

    {-# INLINE liftIO #-}

    liftIO = lift . liftIO


instance MonadReader r m => MonadReader r (EvaluationT r m) where

    {-# INLINEABLE local #-}
    {-# INLINE ask       #-}
    {-# INLINE reader    #-}

    ask     = lift ask

    local f = EvaluationT . RWS.local f . unwrapEvaluationT

    reader  = lift . reader


instance MonadTrans (EvaluationT r) where

    {-# INLINE lift #-}

    lift = EvaluationT . lift . fmap pure


instance Monad m => MonadZip (EvaluationT r m) where

    {-# INLINEABLE mzip     #-}
    {-# INLINEABLE munzip   #-}
    {-# INLINE     mzipWith #-}

    mzip     = liftA2 (,)

    mzipWith = liftA2

    munzip !x = (fst <$> x, snd <$> x)


instance Monad m => Semigroup (EvaluationT r m a) where

    {-# INLINE (<>) #-}

    x <> y = EvaluationT $ liftA2 (<>) (unwrapEvaluationT x) (unwrapEvaluationT y)


-- |
-- Run the 'Evaluation' computation.
runEvaluation :: r -> Evaluation r a -> (Seq Notification, EvaluationResult a)
runEvaluation r = runIdentity . runEvaluationT r


-- |
-- Run the monad transformer for the 'EvaluationT' computation.
runEvaluationT :: Monad m => r -> EvaluationT r m a -> m (Seq Notification, EvaluationResult a)
runEvaluationT r = fmap swap . (\e -> evalRWST e r ()) . unwrapEvaluationT


-- |
-- Fail and indicate the phase in which the failure occured.
failWithPhase :: (Monad m, TextShow s) => ErrorPhase -> s -> EvaluationT r m a
failWithPhase p = EvaluationT . pure . evalUnitWithPhase p


-- |
-- Prints an 'IO' parameterized transformer of 'Evaluation' context to
-- the STDOUT.
showRun :: Show a => r -> EvaluationT r IO a -> IO ()
showRun r = (print . fst =<<) . (\e -> evalRWST e r ()) . unwrapEvaluationT


{-
-- |
-- Map between two `EvaluationT` computations.
mapEvaluationT :: (m (Evaluation a) -> n (Evaluation b)) -> EvaluationT r m a -> EvaluationT r n b
mapEvaluationT f = EvaluationT . f . unwrapEvaluationT
-}


bind :: Monad m => EvaluationT r m a -> (a -> EvaluationT r m b) -> EvaluationT r m b
bind x f = EvaluationT $ do
    y <- unwrapEvaluationT x
    case runEvaluationResult y of
      Left  s -> pure . EU $ Left s
      Right v -> unwrapEvaluationT (f v)


apply :: Monad m => EvaluationT r m (t -> a) -> EvaluationT r m t -> EvaluationT r m a
apply lhs rhs = EvaluationT $ do
    x <- unwrapEvaluationT lhs
    case runEvaluationResult x of
      Left  s -> pure . EU $ Left s
      Right f -> do
          y <- unwrapEvaluationT rhs
          pure . EU $ case runEvaluationResult y of
                        Left  s -> Left s
                        Right v -> Right $ f v


propogate :: Monad m => EvaluationT r m a -> EvaluationT r m b -> EvaluationT r m b
propogate lhs rhs = EvaluationT $ do
    x <- unwrapEvaluationT lhs
    case runEvaluationResult x of
      Left  s -> pure . EU $ Left s
      Right _ -> unwrapEvaluationT rhs


appendNote
  :: ( IsString s
     , Monad m
     )
  => (s -> Notification)
  -> EvaluationT r m a
  -> String -> EvaluationT r m a
appendNote f x = \case
    [] -> x
    ys -> let rwst = unwrapEvaluationT x
          in  EvaluationT $ rwst >>= \e ->
                case runEvaluationResult e of
                  Left  _ -> rwst
                  _       -> let note = pure . f $ fromString ys
                             in  rwst <* RWS.tell note

