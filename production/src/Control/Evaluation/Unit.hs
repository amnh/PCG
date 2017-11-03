-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Evaluation.Unit
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The core monoidal state of an 'Evaluation' monad.
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-} -- , Strict, StrictData #-}

module Control.Evaluation.Unit where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (MonadPlus(mzero, mplus))
import Data.Foldable
import Data.List.NonEmpty
import Data.Monoid   ()
import Data.Semigroup
import GHC.Generics
import Test.QuickCheck


-- |
-- The internal state of the computation. A short-circuiting evaluation unit
-- which returns a value, and error, or indicated that no work was done
data EvalUnit a
   = NoOp
   | Error String 
   | Value a
   deriving (Eq, Generic, Show)


-- | (✔)
instance Alternative EvalUnit where

    empty = mempty

    Value x <|> _    = Value x
    e       <|> NoOp = e
    _       <|> e    = e


-- | (✔)
instance Applicative EvalUnit where

    pure = Value

    NoOp    <*> _ = NoOp
    Error x <*> _ = Error x
    Value f <*> x = f <$> x


-- | (✔)
instance Arbitrary a => Arbitrary (EvalUnit a) where

    arbitrary = oneof [pure mempty, pure $ fail "Error Description", pure <$> arbitrary]


-- | (✔)
instance Functor EvalUnit where

    _ `fmap` NoOp    = NoOp
    _ `fmap` Error x = Error x
    f `fmap` Value x = Value $ f x


instance NFData a => NFData (EvalUnit a)


-- | (✔)
instance Monad EvalUnit where

    return = pure

    fail   = Error

    Error x >>  _ = Error x
    _       >>  e = e

    NoOp    >>= _ = NoOp
    Error x >>= _ = Error x
    Value x >>= f = f x


-- | (✔)
instance MonadPlus EvalUnit where

    mzero = mempty

    mplus = (<>)


-- | (✔)
instance Monoid (EvalUnit a) where

    mempty  = NoOp

    mappend = (<>)

--    mconcat = foldl' (<>) mempty


-- | (✔)
instance Semigroup (EvalUnit a) where

    NoOp    <> e    = e
    e       <> NoOp = e
    Error x <> _    = Error x
    Value _ <> e    = e

--    sconcat (x:|xs) = foldl' (<>) x xs

