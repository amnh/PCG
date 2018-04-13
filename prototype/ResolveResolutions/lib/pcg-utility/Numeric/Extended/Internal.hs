-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Extended.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type-class for extending types that represent finite numerical values to
-- include an ininity representation.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Numeric.Extended.Internal
  ( ExtendedNumber(..)
  , Finite
  ) where


-- |
-- The finite type underlying the infinite extension type.
type family Finite (f :: *)


-- |
-- Conversion two and from a finite and infinite value domains.
--
-- The type-class definition holds an infinity value. Conversion from the
-- extended type back to the finite type is inherently unsafe because you could
-- attempt to coerce the infinity value to a finite value. It is the
-- responsibility of the caller of 'unsafeToFinite' to first check if the value
-- is equal to 'infinity'. This can be done with a value-level equality check
-- '(==)'.
class ExtendedNumber n where

    unsafeToFinite :: n -> Finite n

    fromFinite :: Finite n -> n

    infinity :: n


type instance Finite Word = Word


instance ExtendedNumber Word where

    unsafeToFinite = id

    fromFinite = id

    infinity = maxBound
