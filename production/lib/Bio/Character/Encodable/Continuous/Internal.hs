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

--import Data.Double


-- |
-- A newtype wrapper for a possibly missing continuous.
newtype ContinuousChar = CC (Maybe Double)
  deriving (Eq,Ord)


-- | (✔)
instance Show ContinuousChar where

    show (CC  Nothing) = "?"
    show (CC (Just x)) = show x


-- | (✔)
instance PossiblyMissingCharacter ContinuousChar where

    {-# INLINE toMissing #-}
    toMissing = const $ CC Nothing

    {-# INLINE isMissing #-}
    isMissing (CC Nothing) = True
    isMissing _            = False


-- | (✔)
instance ContinuousCharacter ContinuousChar where

    toContinuousCharacter = CC . fmap (fromRational . toRational)


