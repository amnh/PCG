module Data.ExtendedReal
 ( ExtendedReal()
 , infinity
 , toDouble
 , fromDouble
 ) where


import Control.Applicative (liftA2)
import Data.Ratio
import Data.Maybe          (fromMaybe)


-- | A non-negative real number extended to include infinity, where infinity == maxBound.
newtype ExtendedReal = Cost (Maybe Double)


type family Finite (f :: *)


class ExtendedNumber n where

    unsafeToFinite :: n -> Finite n


-- | A synonym for 'maxBound'
infinity :: ExtendedReal
infinity = maxBound


toDouble :: ExtendedReal -> Double
toDouble (Cost x) = fromMaybe (read "infinity" :: Double) x


fromDouble :: Double -> ExtendedReal
fromDouble = Cost . Just


instance Show ExtendedReal where

    show (Cost input) = maybe "∞" show input


instance Bounded ExtendedReal where

    maxBound = Cost Nothing

    minBound = Cost $ Just 0.0


instance Num ExtendedReal where

  (Cost lhs) + (Cost rhs) = Cost $ liftA2 (+) lhs rhs

  (Cost lhs) - (Cost rhs) = Cost $ liftA2 (-) lhs rhs

  (Cost lhs) * (Cost rhs) = Cost $ liftA2 (*) lhs rhs

  abs = id

  signum (Cost (Just x)) = Cost . Just $ signum x -- the second signum is Double.signum
  signum               _ = 1

  fromInteger = Cost . Just . fromInteger

  negate = id


instance Eq ExtendedReal where

    (Cost lhs) == (Cost rhs) = lhs == rhs


instance Ord ExtendedReal where

    (Cost lhs) <= (Cost rhs) =
        case (lhs, rhs) of
            (Nothing, Nothing) -> True
            (Nothing, Just _ ) -> False
            (Just _,  Nothing) -> True
            (Just x,  Just y ) -> x <= y

    (Cost lhs) < (Cost rhs) =
        case lhs of
            Nothing -> False
            Just x  ->
                case rhs of
                    Nothing -> True
                    Just y  -> x < y

    (Cost lhs) > (Cost rhs) =
        case rhs of
            Nothing -> False
            Just x  ->
                case lhs of
                    Nothing -> True
                    Just y  -> x > y


-- TODO: maybe remove this? 
instance Enum ExtendedReal where

    fromEnum (Cost x) = maybe (maxBound :: Int) fromEnum x

    toEnum = Cost . Just . toEnum


instance Real ExtendedReal where

    toRational (Cost x) = maybe (1%0) toRational x


instance Fractional ExtendedReal where

    (Cost lhs) / (Cost rhs) =
        case (lhs, rhs) of
          (Nothing,       _) -> Cost Nothing
          (Just _ , Nothing) -> Cost $ Just 0.0
          (Just x , Just y ) -> Cost . Just $ x / y

    recip (Cost x) = Cost $ recip <$> x
    
    fromRational = Cost . Just . fromRational

