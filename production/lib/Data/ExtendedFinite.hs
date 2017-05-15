-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ExtendedFinite
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

module Data.ExtendedFinite
  ( ExtendedNumber(..)
  , Finite
  ) where


-- |
-- The finite type underlying the infinite extension type.
type family Finite (f :: *)


-- |
-- Conversion two and from a finite and infinite value domains.
class ExtendedNumber n where

    unsafeToFinite :: n -> Finite n

    fromFinite :: Finite n -> n

    infinity :: n


type instance Finite Word = Word


instance ExtendedNumber Word where

    unsafeToFinite = id

    fromFinite = id

    infinity = maxBound
