{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Evaluation.Internal where

import Control.Applicative
import Control.Evaluation.Unit
import Control.Monad             (MonadPlus(mzero, mplus))
import Control.Monad.Logger
import Data.DList                (DList, toList)
import Data.Monoid
import Test.QuickCheck

-- | Represents the types of contextual notifications that can be generated by
--   the evaluation.
data Notification
   = Warning String
   | Information String
   deriving (Eq,Show)

-- | Reprsents a monoidal evaluations of a value 'a' with contextual notifications.
--   Stores a list of ordered contextual notifications retrievable by 'notifications'.
--   Holds the evaluation state 'a' retrievable 'evaluationResult'.
data Evaluation a
   = Evaluation (DList Notification) (EvalUnit a)
   deriving (Eq)

-- | Retrieve the ordered list of contextual 'Notification's from the 'Evaluation'.
notifications :: Evaluation a -> [Notification]
notifications (Evaluation ms _) = toList ms

-- | Retrieve the result state from the 'Evaluation'.
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
  (<*>) (Evaluation ms x) (Evaluation ns y) = Evaluation (ms <> ns) (x <*> y)

instance Monad Evaluation where
  return = pure
  fail   = Evaluation mempty . Error
  (>>)  (Evaluation ms x) (Evaluation ns y) = Evaluation (ms <> ns) (x>>y)
  (>>=) (Evaluation ms  NoOp    ) _ = Evaluation ms NoOp
  (>>=) (Evaluation ms (Error x)) _ = Evaluation ms $ Error x
  (>>=) (Evaluation ms (Value x)) f = f x `prependNotifications` ms

instance MonadPlus Evaluation where
  mzero = mempty
  mplus = (<>)
  
instance Monoid (Evaluation a) where
  mempty = Evaluation mempty NoOp
  mappend (Evaluation ms x) (Evaluation ns y) = Evaluation (ms <> ns) (x<>y)

-- Maybe add error strings Notifications list also?
-- Currently we throw this information away,
-- perhaps it should be preserved in the Alternative context?
instance Alternative Evaluation where
  empty = mempty
  (<|>) v@(Evaluation _ (Value _)) _                    = v
  (<|>) (Evaluation ms e)          (Evaluation ns NoOp) = Evaluation (ms <> ns) e
  (<|>) (Evaluation ms _)          (Evaluation ns e   ) = Evaluation (ms <> ns) e

instance Logger Evaluation a where
  info s = Evaluation (pure $ Information s) mempty
  warn s = Evaluation (pure $ Warning     s) mempty

-- | Prepends a 'DList' of Notifications to the evaluation. Should only be used
--   internally. 
prependNotifications :: Evaluation a -> DList Notification -> Evaluation a
prependNotifications (Evaluation ms x) ns = Evaluation (ns <> ms) x
