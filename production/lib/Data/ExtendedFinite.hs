
{-# LANGUAGE TypeFamilies #-}

module Data.ExtendedFinite
 ( ExtendedNumber(..)
 , Finite
 ) where


type family Finite (f :: *)


class ExtendedNumber n where

    unsafeToFinite :: n -> Finite n

    fromFinite :: Finite n -> n

    infinity :: n


type instance Finite Word = Word


instance ExtendedNumber Word where

    unsafeToFinite = id

    fromFinite = id

    infinity = maxBound


