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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Parallel.Custom
  ( parmap
  , parZipWith
  ) where


import Control.Parallel.Strategies
import Data.Key
import Prelude hiding (zipWith)


-- |
-- Map a function over a traversable structure in parrallel.
-- Should be prefered over 'parMap' which is fixed to lists.
parmap :: Traversable t => Strategy b -> (a -> b) -> t a -> t b
parmap strat f = withStrategy (parTraversable strat) . fmap f


-- |
-- Zip two traversable, zippable structures in parrallel with a function. 
parZipWith :: (Traversable t, Zip t) => Strategy c -> (a -> b -> c) -> t a -> t b -> t c
parZipWith strat f lhs rhs = withStrategy (parTraversable strat) $ zipWith f lhs rhs
