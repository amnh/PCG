{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Evaluation.Internal where

import Control.Applicative
import Control.Monad             (MonadPlus(mzero, mplus))
import Control.Evaluation.Unit
--import Control.Monad.Fix         (MonadFix(mfix))
--import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except (ExceptT(..))
import Data.DList                (DList,append,singleton,toList)
import Data.Monoid
import Test.QuickCheck

data Notification
   = Warning String
   | Information String
   deriving (Eq,Show)

data Evaluation a
   = Evaluation (DList Notification) (EvalUnit a)
   deriving (Eq)

notifications :: Evaluation a -> [Notification]
notifications (Evaluation ms _) = toList ms

evaluationResult :: Evaluation a -> EvalUnit a
evaluationResult (Evaluation _ x) = x
                                  
instance Arbitrary a => Arbitrary (Evaluation a) where
  arbitrary = oneof [pure mempty, pure $ fail "Error Description", pure <$> arbitrary]

                                  
instance Show a => Show (Evaluation a) where
  show (Evaluation ms x) = unwords
                          [ "Evaluation"
                          , show $ toList ms
                          , show x
                          ]

instance Functor Evaluation where
  fmap f (Evaluation ms x) = Evaluation ms (f <$> x)

instance Applicative Evaluation where
  pure = Evaluation mempty . pure
  (<*>) (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x <*> y)

instance Monad Evaluation where
  return = pure
  fail   = Evaluation mempty . Error
  (>>)  (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x>>y)
  (>>=) (Evaluation ms  NoOp    ) _ = Evaluation ms NoOp
  (>>=) (Evaluation ms (Error x)) _ = Evaluation ms $ Error x
  (>>=) (Evaluation ms (Value x)) f = f x `prependNotifications` ms

instance MonadPlus Evaluation where
  mzero = mempty
  mplus = (<>)
  
instance Monoid (Evaluation a) where
  mempty = Evaluation mempty NoOp
  mappend (Evaluation ms x) (Evaluation ns y) = Evaluation (ms `append` ns) (x<>y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Alternative Evaluation where
  empty = mempty
  (<|>) v@(Evaluation _ (Value _)) _                     = v
  (<|>) (Evaluation ms e)          (Evaluation ns NoOp) = Evaluation (ms `append` ns) e
  (<|>) (Evaluation ms _)          (Evaluation ns e   ) = Evaluation (ms `append` ns) e

{-|
Typeclass Laws:

Failure nullification:
  fail x >> info y === fail x
  fail x >> warn y === fail x

Assocativity:
  info x >> (info y >> info z) === (info x >> info y) >> info z
  warn x >> (warn y >> warn z) === (warn x >> warn y) >> warn z

-}
class Monad m => Logger m a where
  info, warn   :: String -> m a
  (<?>), (<!>) :: m a -> String -> m a
  (<?>) x s = x >> info s
  (<!>) x s = x >> warn s

instance Logger Evaluation a where
  info s = Evaluation (singleton $ Information s) mempty
  warn s = Evaluation (singleton $ Warning     s) mempty

prependNotifications :: Evaluation a -> DList Notification -> Evaluation a
prependNotifications e@(Evaluation _  (Error _)) _  = e
prependNotifications   (Evaluation ms x        ) ns = Evaluation (ns `append` ms) x
