-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Continuous.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Encodable.Continuous.Internal where

import Bio.Character.Encodable.Continuous.Class
import Bio.Character.Encodable.Internal
import Control.Arrow ((&&&))
import Data.Range
import Numeric.Extended.Real


-- |
-- Represents a real-valued range with a minimum lower bound of zero and a
-- maximum upper bound of infinity.
newtype ContinuousChar = CC (ExtendedReal, ExtendedReal)
  deriving (Eq)


-- | (✔)
instance Show ContinuousChar where

    show (CC (lower, upper))
      | lower == upper = show lower
      | otherwise      = renderRange lower upper
      where
        renderRange x y = mconcat [ "[", show x, ", ", show y, "]" ]


-- | (✔)
instance PossiblyMissingCharacter ContinuousChar where

    {-# INLINE toMissing #-}
    toMissing = const $ CC missingRange

    {-# INLINE isMissing #-}
    isMissing (CC c) = c == missingRange


-- |
-- The default range for "missing" continuous characters.
--
-- This value ensures that the scoring on th character produces correct results.
missingRange :: (ExtendedReal, ExtendedReal)
missingRange = (minBound, maxBound)

-- -- | (✔)
-- instance PossiblyMissingCharacter ContinuousChar where

--     {-# INLINE toMissing #-}
--     toMissing = const $ CC Nothing

--     {-# INLINE isMissing #-}
--     isMissing (CC Nothing) = True
--     isMissing _            = False


-- | (✔)
instance ContinuousCharacter ContinuousChar where

    toContinuousCharacter = CC . maybe missingRange (f &&& f)
      where
        f = fromRational . toRational


type instance Bound ContinuousChar = ExtendedReal


-- | (✔)
instance Ranged ContinuousChar where

    toRange (CC interval) = fromTuple interval

    fromRange interval = CC (lowerBound interval, upperBound interval)

    zeroRange _ = fromTuple (0,0)

