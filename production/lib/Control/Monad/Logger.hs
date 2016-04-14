-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logger
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A monadic extension classs that enables the support of logging.
-- TODO: Should be combined with MonadFail when GHC 8.0 is released
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.Logger
  ( Logger(..)
  ) where

{-|
'Typeclass Laws:

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
