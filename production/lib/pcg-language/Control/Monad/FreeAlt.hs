{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.FreeAlt
  ( FreeAlt()
  , FreeAlt2
  , FreeAlt3(..)
  ) where

import Control.Applicative
--import Control.Alternative.Free
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Functor.Alt


newtype FreeAlt3 f a = FA3 { unFA :: Control.Monad.Free.Free f a }
    deriving (Functor)


instance (Functor f) => Alt (FreeAlt3 f) where

    lhs <!> _rhs = lhs

    
instance Functor f => Applicative (FreeAlt3 f) where

    pure = FA3 . pure

    (FA3 f) <*> (FA3 x) = FA3 $ f <*> x
    

instance Functor f => Monad (FreeAlt3 f) where

    (FA3 m) >>= f = FA3 $ m >>= unFA . f


instance Functor f => MonadFree f (FreeAlt3 f) where

    wrap = FA3 . wrap . fmap unFA


newtype FreeAlt f a = FA { unwrap :: FreeT f Maybe a }
    deriving (Functor)


toFreeAlt :: Maybe (FreeF f a (FreeT f Maybe a)) -> FreeAlt f a
toFreeAlt = FA . FreeT


toMaybe :: FreeAlt f a -> Maybe (FreeF f a (FreeT f Maybe a))
toMaybe   = runFreeT . unwrap


instance (Functor f {- , Alternative f -}) => Alternative (FreeAlt f) where

    lhs <|> rhs = toFreeAlt $ toMaybe lhs <|> toMaybe rhs

    empty = toFreeAlt Nothing

    
instance Functor f => Applicative (FreeAlt f) where

    pure = FA . pure

    (FA f) <*> (FA x) = FA $ f <*> x
    

instance Functor f => Monad (FreeAlt f) where

    (FA m) >>= f = FA $ m >>= unwrap . f


instance Functor f => MonadFree f (FreeAlt f) where

    wrap = FA . wrap . fmap unwrap


data  FreeAlt2 f a
    = Pure2 a
    | Free2 (f (FreeAlt2 f a))
    | Empty2
    deriving (Functor)


instance (Functor f {- , Alternative f -}) => Alternative (FreeAlt2 f) where

    Empty2 <|> x = x
    x     <|> _ = x

    empty = Empty2

    
instance Functor f => Applicative (FreeAlt2 f) where

    pure = Pure2

    Empty2   <*>       _  = Empty2
    Pure2 _  <*> Empty2   = Empty2
    Pure2 a  <*> Pure2 b  = Pure2 $ a b
    Pure2 a  <*> Free2 mb = Free2 $ fmap a  <$> mb
    Free2 ma <*>       b  = Free2 $ (<*> b) <$> ma


instance Functor f => Monad (FreeAlt2 f) where

    return = pure

    Empty2  >>= _ = Empty2
    Pure2 a >>= f = f a
    Free2 m >>= f = Free2 ((>>= f) <$> m)


instance Functor f => MonadFree f (FreeAlt2 f) where

    wrap = Free2


{-
iter :: Functor f => (f a -> a) -> FreeAlt2 f a -> a
iter = undefined


iterA :: (Applicative p, Functor f) => (f (p a) -> p a) -> FreeAlt2 f a -> p a
iterA = undefined


iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> FreeAlt2 f a -> m a
iterM = undefined
-}


{-
data  FreeAlt f a
    = FreeM (Free f a)
    | FreeA (Alt  f a)
    deriving (Functor)


instance Functor f => Applicative (FreeAlt f) where

   pure = FreeM . pure

   (<*>) x y =
     case (x, y) of
       (FreeM f, FreeM m) -> FreeM $ f <*> m
       (FreeA f, FreeA a) -> FreeA $ f <*> a
--       (FreeM f, FreeA a) -> runAlt (FreeM . pure) (liftAlt f <*> a)
       (FreeM f, FreeA a) -> FreeM $ (\z -> runAlt id $ liftAlt z <*> a) <$> f



-}
