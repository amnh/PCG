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


import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as F

{- |

  A 'MonadFail' that has been extended to support "information" and "warning"
  level messages.

  Typeclass Laws:

  Failure nullification:

 > fail x <?> y === fail x
 > fail x <@> y === fail x

  Assocativity:

 > let a = v <?> x in a <?> y <?> z === let a = v <?> x <?> y in a <?> z
 > let a = v <@> x in a <@> y <@> z === let a = v <@> x <@> y in a <@> z

-}
class MonadFail m => Logger m a where

    (<?>), (<@>), (<☓>) :: m a -> String -> m a

    (<☓>) x s = x *> F.fail s

