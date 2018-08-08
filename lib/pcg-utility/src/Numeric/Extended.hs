-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Extended
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Numeric.Extended
  ( ExtendedNatural()
  , ExtendedNumber(..)
  , ExtendedReal()
  , Finite
  ) where

import           Numeric.Extended.Internal
import           Numeric.Extended.Natural
import           Numeric.Extended.Real
