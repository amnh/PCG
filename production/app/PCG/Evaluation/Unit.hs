module PCG.Evaluation.Unit where

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM2, ap)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Monoid

data EvalUnit a
   = NoOp
   | Error String
   | Value a
   deriving (Eq,Show)

instance Functor EvalUnit where
  _ `fmap` NoOp    = NoOp
  _ `fmap` Error x = Error x
  f `fmap` Value x = Value $ f x

instance Applicative EvalUnit where
  pure = Value
  NoOp    <*> _ = NoOp
  Error x <*> _ = Error x
  Value f <*> x = f <$> x

instance Monad EvalUnit where
  return = pure
  fail   = Error
  Error x >>  _ = Error x
  _       >>  e = e
  NoOp    >>= f = NoOp
  Error x >>= f = Error x
  Value x >>= f = f x

instance MonadPlus EvalUnit where
  mzero = mempty
  mplus = (<>)

instance Monoid (EvalUnit a) where
  mempty  = NoOp
  NoOp    `mappend` e    = e
  e       `mappend` NoOp = e
  Error x `mappend` _    = Error x
  Value _ `mappend` e    = e

instance Alternative EvalUnit where
  empty = mempty
  Value x <|> _    = Value x
  e       <|> NoOp = e
  _       <|> e    = e

