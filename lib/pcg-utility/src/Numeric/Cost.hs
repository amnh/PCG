-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Cost
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A /non-negative/, rational number extended to include infinity where:
--
--   * Non-negative
--   * Monotonically non-decreasing "addition"
--   * Equatable
--   * Total ordering
--   * An "infinite value" representation
--   * Perfect precision for rational numbers
--   * Reasonable efficiency
--   * Monoid under addition
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}


module Numeric.Cost
  ( Cost
  , ExtendedNumber(..)
  , Finite
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Binary
import Data.Data
import Data.Foldable
import Data.List                 (intersperse)
import Data.Ratio
import Data.Sequence             hiding (intersperse, length)
import Data.String
import GHC.Generics
import Numeric.Extended.Internal
import Numeric.Natural
import Prelude                   hiding (reverse, splitAt)
import Test.QuickCheck
import TextShow                  (TextShow (showb))


-- |
-- A /non-negative/, rational number extended to include infinity.
data Cost = Cost !Natural !Natural
    deriving stock    (Data, Generic, Typeable)
    deriving anyclass (NFData)


-- |
-- The finite value of a 'Cost' would be more correctly represented as 'Ratio'
-- 'Natural'.
--
-- However, it is much easier to interact with the Haskell ecosystem
-- if 'Rational' is used. If a negative number is supplied to 'fromFinite', the
-- additive identity (zero, 'mempty') is returned instead to enforce the
-- non-negativity constraint on 'Cost'.
type instance Finite Cost = Rational


instance Arbitrary Cost where

    arbitrary = do
        n <- choose weightForInfinity
        if n == 0
        then pure infinity
        else do
           num <- arbitrary :: Gen Integer
           dem <- arbitrary `suchThat` (> 0) :: Gen Integer
           let num' = fromIntegral $ abs num
           let dem' = fromIntegral dem
           let d = gcd num' dem'
           pure . Cost (num' `quot` d) $ dem' `quot` d
      where
        -- We generate the 'infinity' value 1 in 20 times.
        weightForInfinity = (0, 19) :: (Word, Word)


instance Binary Cost


instance Eq Cost where

    (Cost n1 d1) == (Cost n2 d2) =
       d1 == d2 && (d1 == 0 || n1 == n2)


instance ExtendedNumber Cost where

    unsafeToFinite (Cost _ 0) = error "Attempting to convert an infinite Cost to a finite Rational."
    unsafeToFinite (Cost n d) = toInteger n % toInteger d

    fromFinite r
      | numerator r < 0 = mempty
      | otherwise = Cost <$> fromIntegral . numerator <*> fromIntegral . denominator $ r

    infinity   = Cost 1 0


instance Monoid Cost where

    mempty = Cost 0 1


instance Ord Cost where

   (Cost n1 d1) `compare` (Cost n2 d2) =
       -- Special case the infinity values
       case (0 == d1, 0 == d2) of
         (True , True ) -> EQ
         (True , False) -> GT
         (False, True ) -> LT
         (False, False) -> (n1 % d1) `compare` (n2 % d2)


instance Semigroup Cost where

    (<>) lhs@(Cost _ 0) _ = lhs
    (<>) _ rhs@(Cost _ 0) = rhs
    (<>) (Cost n1 d1) (Cost n2 d2) = Cost (n' `quot` x) $ d' `quot` x
      where
        n' = n1*d2 + n2*d1
        d' = d1 * d2
        x  = gcd n' d'


instance Show Cost where

    show = renderCost


instance TextShow Cost where

    showb = fromString . show


renderCost :: Cost -> String
renderCost (Cost   _   0) = "∞"
renderCost (Cost num den) = renderRational num den


renderRational :: Natural -> Natural -> String
renderRational num den =
    case num `quotRem` den of
      (q,0) -> show q
      (q,r) -> fold [ show q, ".", toList (go (if q == 0 then num else r) mempty)]
  where
    go  0 xs = foldMap (show . fst) xs
    go !v xs =
      case hasCycle xs of
        Nothing                  -> let t@(_,r) = (v*10) `quotRem` den in go r $ xs |> t
        Just (static, repeating) -> render static <> addOverline (render repeating)

    addOverline = (<>[c]) . intersperse c
      where
        -- Unicode literal for the "combining overline character".
        -- Places an overline above the character /before/ itself in the string.
        !c = '\x0305'

    render = foldMap (show . fst)

    hasCycle x = foldl' f Nothing . reverse . dropEmptySeq $ tails x
      where
        !n = length x
        f a e = a <|> let !m = length e
                          !(s,r) = splitAt (n-(2*m)) x
                      in  if r == e <> e
                          then Just (s, e)
                          else Nothing

    dropEmptySeq :: Seq a -> Seq a
    dropEmptySeq  xs@Empty  = xs
    dropEmptySeq (xs :|> _) = xs
