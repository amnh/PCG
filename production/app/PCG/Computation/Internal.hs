{-# LANGUAGE DeriveFunctor #-}
module PCG.Computation.Internal where

import Control.Applicative
import Control.Monad             (MonadPlus(mzero, mplus), liftM2)
--import Control.Monad.Fix         (MonadFix(mfix))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except (ExceptT(..))
import Data.DList                (DList,append,apply,singleton,toList)
import qualified Data.DList as D (empty)
import Data.Functor.Classes
import Data.Monoid

import PCG.Computation.Unit

data Notification
   = Warning String
   | Information String
   deriving (Show)

data Computation a
   = Computation (DList Notification) (CompUnit a)

notifications :: Computation a -> [Notification]
notifications (Computation ms _) = toList ms

instance Show a => Show (Computation a) where
  show (Computation ms e) = unwords
                          [ "Computation"
                          , show $ toList ms
                          , show e
                          ]

instance Functor Computation where
  fmap f (Computation ms x) = Computation ms (f <$> x)

instance Applicative Computation where
  pure = Computation D.empty . pure
  (<*>) (Computation ms x) (Computation ns y) = Computation (ms `append` ns) (x <*> y)

instance Monad Computation where
  return = pure
  fail   = Computation D.empty . Error
  (>>)  (Computation ms x) (Computation ns y) = Computation (ms `append` ns) (x>>y)
  (>>=) (Computation ms  NoOp    ) _ = Computation ms $ NoOp
  (>>=) (Computation ms (Error x)) _ = Computation ms $ Error x
  (>>=) (Computation ms (Value x)) f = f x `prependNotifications` ms

instance MonadPlus Computation where
  mzero = mempty
  mplus = (<>)
  
instance Monoid (Computation a) where
  mempty = Computation D.empty NoOp
  mappend (Computation ms x) (Computation ns y) = Computation (ms `append` ns) (x<>y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Alternative Computation where
  empty = mempty
  (<|>) v@(Computation ms (Value x)) _                     = v
  (<|>) (Computation ms e)           (Computation ns NoOp) = Computation (ms `append` ns) e
  (<|>) (Computation ms _)           (Computation ns e   ) = Computation (ms `append` ns) e

(<!>), (<?>) :: Computation a -> String -> Computation a
c <!> s = c <> (warn s)
c <?> s = c <> (info s)

info, warn :: String -> Computation a
info s = Computation (singleton $ Information s) mempty
warn s = Computation (singleton $ Warning     s) mempty

prependNotifications :: Computation a -> DList Notification -> Computation a
prependNotifications e@(Computation _  (Error _)) _  = e
prependNotifications   (Computation ms x        ) ns = Computation (ns `append` ms) x
