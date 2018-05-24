-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for collecting 'Left' values of 'Either' forming a 'Validation'
-- similar context.
--
-----------------------------------------------------------------------------

module Data.Either.Custom where

import Control.Monad.Trans.Except
import Data.Either        (partitionEithers)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Allows for the coalescence of 'Left' values that form a 'Semigroup'.
-- Overrides short-circuiting behaviour of the standard monadic 'sequence'
-- definition. The validation context will evaluate the entire
-- @['Either' e a]@ returning a Right value if and only if only Right values were
-- present in the list of 'Either's. If any @Left@ values were present in the
-- list of 'Either's then a 'Left' value is returned consisting of the an order
-- preserving fold using the 'Semigroup' operation @('<>')@.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> eitherValidation [Right 1, Right 2]
-- Right [1,2]
--
-- >>> eitherValidation [Right 1, Left "Hello", Right 2]
-- Left "Hello"
--
-- >>> eitherValidation [Right 1, Left "Hello", Right 2, Left "Good", Left "bye"]
-- Left "HelloGoodbye"
--
-- >>> eitherValidation [Left ("Love", "Hate"), Left (" you", " me")]
-- Left ("Love you", "Hate me")
eitherValidation :: (Foldable t, Semigroup e) => t (Either e a) -> Either e [a]
eitherValidation xs =
  case partitionEithers $ toList xs of
    ([]  , r) -> Right r
    (e:es, _) -> Left . sconcat $ e:|es


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Works similarly to 'eitherValidation' but within the 'MonadTrans' context.
eitherTValidation :: (Foldable t, Monad m, Semigroup e) => t (ExceptT e m a) -> ExceptT e m [a]
eitherTValidation = ExceptT . fmap eitherValidation . traverse runExceptT . toList
