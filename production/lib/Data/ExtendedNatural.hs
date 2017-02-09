module Data.ExtendedNatural
 ( ExtendedNatural()
 , infinityCost
 , minCost
 , toWord
 , fromWord
 ) where


import Control.Applicative (liftA2)
import Data.Maybe          (fromMaybe)


-- | A natural number extended to include infinity. Where infinity == maxBound
newtype ExtendedNatural = Cost (Maybe Word)

infinityCost :: ExtendedNatural
infinityCost = maxBound :: ExtendedNatural

minCost :: ExtendedNatural
minCost = minBound :: ExtendedNatural

toWord :: ExtendedNatural -> Word
toWord (Cost x) = fromMaybe (maxBound :: Word) x

fromWord :: Word -> ExtendedNatural
fromWord = Cost . Just


instance Show ExtendedNatural where
    show input =
        case input of
            Cost (Just c) -> show c
            _      -> show "Nothing"


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

    (Cost lhs) <= (Cost rhs) =
        case (lhs, rhs) of
            (Nothing, Nothing) -> True
            (Nothing, Just _)  -> False
            (Just _,  Nothing) -> True
            (Just x,  Just y)  -> x <= y

    (Cost lhs) < (Cost rhs) =
        case lhs of
            Nothing  -> False
            (Just x) ->
                case rhs of
                    Nothing  -> True
                    (Just y) -> x <= y




instance Enum ExtendedNatural where

    fromEnum (Cost x) = maybe (maxBound :: Int) fromEnum x

    toEnum = Cost . Just . toEnum

{-}

instance Eq ExtendedNatural where

    (Cost lhs) == (Cost rhs)
        | lhs == Nothing && rhs == Nothing = True
        | lhs == Nothing                   = False
        | rhs == Nothing                   = False
        | lhs /= rhs                       = False
        | otherwise                        = True


instance Ord ExtendedNatural where

    (Cost lhs) <= (Cost rhs) = lhs <= rhs

-}

instance Integral ExtendedNatural where

    toInteger (Cost x) = toInteger $ maybe (maxBound :: Word) id x

    quotRem   (Cost lhs) (Cost rhs) =
        case liftA2 quotRem lhs rhs of
          Nothing -> (Cost Nothing, 0)
          Just (q,r) -> (Cost $ Just q, Cost $ Just r)


instance Real ExtendedNatural where

    toRational = toRational . toInteger

