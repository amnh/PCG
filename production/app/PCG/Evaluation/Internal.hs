{-# LANGUAGE DeriveFunctor #-}
module PCG.Evaluation.Internal where

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

import PCG.Evaluation.Unit

data Notification
   = Warning String
   | Information String
   deriving (Show)

data Evaluation a
   = Evaluation (DList Notification) (EvalUnit a)

notifications :: Evaluation a -> [Notification]
notifications (Evaluation ms _) = toList ms

instance Show a => Show (Evaluation a) where
  show (Evaluation ms e) = unwords
                          [ "Evaluation"
                          , show $ toList ms
                          , show e
                          ]

instance Functor Evaluation where
  fmap f (Evaluation ms x) = Evaluation ms (f <$> x)

instance Applicative Evaluation where
  pure = Evaluation D.empty . pure
  (<*>) (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x <*> y)

instance Monad Evaluation where
  return = pure
  fail   = Evaluation D.empty . Error
  (>>)  (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x>>y)
  (>>=) (Evaluation ms  NoOp    ) _ = Evaluation ms $ NoOp
  (>>=) (Evaluation ms (Error x)) _ = Evaluation ms $ Error x
  (>>=) (Evaluation ms (Value x)) f = f x `prependNotifications` ms

instance MonadPlus Evaluation where
  mzero = mempty
  mplus = (<>)
  
instance Monoid (Evaluation a) where
  mempty = Evaluation D.empty NoOp
  mappend (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x<>y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Alternative Evaluation where
  empty = mempty
  (<|>) v@(Evaluation ms (Value x)) _                     = v
  (<|>) (Evaluation ms e)           (Evaluation ns NoOp) = Evaluation (ms `append` ns) e
  (<|>) (Evaluation ms _)           (Evaluation ns e   ) = Evaluation (ms `append` ns) e

(<!>), (<?>) :: Evaluation a -> String -> Evaluation a
c <!> s = c <> (warn s)
c <?> s = c <> (info s)

info, warn :: String -> Evaluation a
info s = Evaluation (singleton $ Information s) mempty
warn s = Evaluation (singleton $ Warning     s) mempty

prependNotifications :: Evaluation a -> DList Notification -> Evaluation a
prependNotifications e@(Evaluation _  (Error _)) _  = e
prependNotifications   (Evaluation ms x        ) ns = Evaluation (ns `append` ms) x
