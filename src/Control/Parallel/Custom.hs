------------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Parallel.Custom
  ( parmap
  , parZipWith
  , parZipWith3
  ) where


import Control.Parallel.Strategies
import Data.Key
import Prelude                     hiding (zipWith)


-- |
-- Map a function over a traversable structure in parrallel.
-- Should be prefered over 'parMap' which is fixed to lists.
parmap :: Traversable t => Strategy b -> (a -> b) -> t a -> t b
parmap _strat f = withStrategy rpar . fmap f


-- |
-- Zip two traversable, zippable structures in parrallel with a function.
parZipWith :: Zip t => Strategy c -> (a -> b -> c) -> t a -> t b -> t c
parZipWith _strat f lhs rhs = withStrategy rpar $ zipWith f lhs rhs


-- |
-- Zip three traversable, zippable structures in parrallel with a function.
parZipWith3 :: Zip t => Strategy d -> (a -> b -> c -> d) -> t a -> t b -> t c -> t d
parZipWith3 _strat f x y z = withStrategy rpar $ zap (zipWith f x y) z
