-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Validation
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Validation' type's monad transformer definition.
--
-- The transformer will collect failure up until the first monadic bind,
-- after which the precence of errors on the left hand side of the bind
-- will cause the computation to short-circuit.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Validation
  ( ValidationT(..)
  , emap
  , invalid
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Fail        (MonadFail)
import qualified Control.Monad.Fail        as F
import           Control.Monad.Fix         (MonadFix (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Zip         (MonadZip (..))
import           Data.Bifunctor
import           Data.Functor.Alt          (Alt (..))
import           Data.Functor.Apply        (Apply (..))
import           Data.Functor.Bind         (Bind (..))
import           Data.Functor.Classes      (Eq1 (..), Ord1 (..), Show1 (..))
import           Data.String
import           Data.Validation
import           GHC.Generics
import           Test.QuickCheck           hiding (Failure, Success)


-- |
-- A monad transformer of 'Evaluation'.
newtype ValidationT e m a
      = ValidationT
      { -- | Run the 'ValidationT' monad transformer
        runValidationT :: m (Validation e a)
      } deriving (Generic)


instance Alt m => Alt (ValidationT e m)  where

    {-# INLINEABLE (<!>) #-}

    (<!>) x y = ValidationT $ runValidationT x <!> runValidationT y


instance  (Monad m, Semigroup e) => Apply (ValidationT e m)  where

    {-# INLINEABLE (<.>) #-}
    {-# INLINE     (.>)  #-}

    (<.>) f v = ValidationT $ do
        x <- runValidationT f
        case x of
          Failure e -> pure $ Failure e
          Success y -> fmap y <$> runValidationT v

    (.>)  x y = ValidationT $ do
        z <- runValidationT x
        case z of
          Failure e -> pure $ Failure e
          Success _ -> runValidationT y


instance (Monad m, Semigroup e) => Applicative (ValidationT e m) where

    {-# INLINEABLE (<*>) #-}
    {-# INLINE     (*>)  #-}
    {-# INLINE     pure  #-}

    pure = ValidationT . pure . Success

    (<*>) = (<.>)

    (*>)  = (*>)


instance (Arbitrary a, Arbitrary e, Arbitrary1 m) => Arbitrary (ValidationT e m a) where

    {-# INLINE arbitrary #-}

    arbitrary = ValidationT <$> liftArbitrary genValidation
      where
        genValidation = arbitrary >>= \success -> if success then Success <$> arbitrary else Failure <$> arbitrary


instance (Apply m, Monad m, Semigroup e) => Bind (ValidationT e m) where

    {-# INLINEABLE (>>-) #-}

    (>>-) v f = ValidationT $ do
        x <- runValidationT v
        case x of
          Failure e -> pure $ Failure e
          Success a -> runValidationT $ f a


instance (Eq a, Eq e, Eq1 m) => Eq (ValidationT e m a) where

    {-# INLINE (==) #-}

    (==) x = liftEq (==) (runValidationT x) . runValidationT


{-
instance Eq1 m => Eq1 (ValidationT e m) where

    {-# INLINE liftEq #-}

    liftEq f lhs = liftEq (liftEq f) (runValidationT lhs) . runValidationT
-}


instance Foldable m => Foldable (ValidationT e m) where

    {-# INLINEABLE foldMap #-}

    foldMap f = foldMap (foldMap f) . runValidationT


instance Functor m => Functor (ValidationT e m) where

    {-# INLINEABLE fmap #-}

    fmap f = ValidationT . fmap (fmap f). runValidationT


instance (Monad m, NFData a, NFData e) => NFData (ValidationT e m a) where

    {-# INLINE rnf #-}

    rnf (ValidationT x) = (force <$> x) `seq` ()


instance (Monad m, Semigroup e) => Monad (ValidationT e m) where

    {-# INLINEABLE (>>=)  #-}
    {-# INLINE     (>>)   #-}
    {-# INLINE     return #-}
    {-# INLINE     fail   #-}

    (>>=) v f = ValidationT $ do
        x <- runValidationT v
        case x of
          Failure e -> pure $ Failure e
          Success a -> runValidationT $ f a

    (>>)   = (*>)

    return = pure

    fail   = error "Use Monad.Fail.fail"


instance (IsString e, Monad m, Semigroup e) => MonadFail (ValidationT e m) where

    {-# INLINE fail #-}

    fail = ValidationT . pure . Failure . fromString


instance (Monad m, Semigroup e) => MonadFix (ValidationT e m) where

    mfix f = let a = a >>= f in a


instance (MonadIO m, Semigroup e) => MonadIO (ValidationT e m) where

    {-# INLINE liftIO #-}

    liftIO = lift . liftIO


instance MonadTrans (ValidationT e) where

    {-# INLINE lift #-}

    lift = ValidationT . fmap Success


instance (Monad m, Semigroup e) => MonadZip (ValidationT e m) where

    {-# INLINEABLE mzip     #-}
    {-# INLINEABLE munzip   #-}
    {-# INLINE     mzipWith #-}

    mzip     = liftA2 (,)

    mzipWith = liftA2

    munzip x = let !v = runValidationT x in (f fst v, f snd v)
      where
        f t = ValidationT . fmap (t . vunzip)
        vunzip (Failure e    ) = (Failure e, Failure e)
        vunzip (Success (a,b)) = (Success a, Success b)


instance (Ord a, Ord e, Ord1 m) => Ord (ValidationT e m a) where

    {-# INLINE compare #-}

    compare x = liftCompare compare (runValidationT x) . runValidationT


{-
instance (Ord a, Ord e, Ord1 m) => Ord (ValidationT e m a) where

    {-# INLINE compare #-}

    compare = liftCompare compare


instance Ord1 m => Ord1 (ValidationT e m) where

    {-# INLINE liftCompare #-}

    liftCompare cmp lhs = liftCompare (liftCompare cmp) (runValidationT lhs) . runValidationT
-}


instance (Applicative m, Semigroup e) => Semigroup (ValidationT e m a) where

    {-# INLINE (<>) #-}

    x <> y = ValidationT $ liftA2 (<>) (runValidationT x) (runValidationT y)


instance (Show a, Show e, Show1 m) => Show (ValidationT e m a) where

    showsPrec n = liftShowsPrec showsPrec showList n . runValidationT


instance Traversable m => Traversable (ValidationT e m) where

    traverse f = fmap ValidationT . traverse (traverse f) . runValidationT


-- |
-- Map over the error value.
emap :: Functor f => (e -> b) -> ValidationT e f a -> ValidationT b f a
emap f = ValidationT . fmap (first f) . runValidationT


-- |
-- Place an error value into the Monad transformer.
invalid :: Applicative f => e -> ValidationT e f a
invalid = ValidationT . pure . Failure
