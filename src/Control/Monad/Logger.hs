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
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Logger
  ( Logger(..)
  ) where


import Control.Monad.Fail

{- |

  A 'MonadFail' that has been extended to support "information" and "warning"
  level messages.

  Typeclass Laws:

  Failure nullification:

 > fail x >> info y === fail x
 > fail x >> warn y === fail x

  Assocativity:

 > info x >> (info y >> info z) === (info x >> info y) >> info z
 > warn x >> (warn y >> warn z) === (warn x >> warn y) >> warn z

-}
class MonadFail m => Logger m a where

    info, warn   :: String -> m a

    (<?>), (<!>) :: m a -> String -> m a

    (<?>) x s = x >> info s

    (<!>) x s = x >> warn s
