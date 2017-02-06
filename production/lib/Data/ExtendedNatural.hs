module Data.ExtendedNatural
 ( ExtendedNatural()
 ) where


import Control.Applicative (liftA2)


-- | A natural number extended to include infinity. Where infinity == maxBound
newtype ExtendedNatural = Cost (Maybe Word)


instance Bounded ExtendedNatural where

    maxBound = Cost Nothing

    minBound = Cost $ Just minBound


instance Num ExtendedNatural where

  (Cost lhs) + (Cost rhs) = Cost $ liftA2 (+) lhs rhs

  (Cost lhs) - (Cost rhs) = Cost $ liftA2 (-) lhs rhs

  (Cost lhs) * (Cost rhs) = Cost $ liftA2 (*) lhs rhs

  abs = id

  signum (Cost (Just 0)) = 0
  signum               _ = 1

  fromInteger = Cost . Just . fromInteger

  negate = id


instance Eq ExtendedNatural where

    (Cost lhs) == (Cost rhs) = lhs == rhs


instance Ord ExtendedNatural where

    (Cost lhs) <= (Cost rhs) = lhs <= rhs


instance Enum ExtendedNatural where

    fromEnum (Cost x) = maybe (maxBound :: Int) fromEnum x

    toEnum = Cost . Just . toEnum


instance Integral ExtendedNatural where

    toInteger (Cost x) = toInteger $ maybe (maxBound :: Word) id x

    quotRem   (Cost lhs) (Cost rhs) =
        case liftA2 quotRem lhs rhs of
          Nothing -> (Cost Nothing, 0)
          Just (q,r) -> (Cost $ Just q, Cost $ Just r)


instance Real ExtendedNatural where

    toRational = toRational . toInteger
