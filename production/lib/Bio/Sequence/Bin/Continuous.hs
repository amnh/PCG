------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Bin.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Sequence.Bin.Continuous
  ( ContinuousBin(..)
  , continuousBin
  ) where


import           Bio.Sequence.SharedContinugousMetatdata
import           Data.Foldable
import           Data.List.NonEmpty hiding (length,toList)
import           Data.Semigroup
import           Data.Monoid               (mappend)
import           Data.Vector               (Vector)
import qualified Data.Vector        as V

-- |
-- A bin of one or more real-valued characters and thier corresponding metadata.
--
-- Use 'continuousBin' and '(<>)' to construct larger bins with differing metadata.
data ContinuousBin
   = ContinuousBin
   { characterStream :: Vector Double
   , metatdataBounds :: SharedMetatdataIntervals
   } deriving (Eq,Show)


instance Semigroup ContinuousBin where

  lhs <> rhs =
    ContinuousBin
      { characterStream = characterStream lhs `mappend` characterStream rhs
      , metatdataBounds = metatdataBounds lhs `mappend` metatdataBounds rhs
      }


-- |
-- Constructs a non-empty bin of continuous character that all shared the same
-- metadata values.
continuousBin :: NonEmpty Double -> GeneralCharacterMetadata -> ContinuousBin
continuousBin continuousCharacters corespondingMetadata =
  ContinuousBin
    { characterStream = V.fromList $ toList continuousCharacters
    , metatdataBounds = singleton (length continuousCharacters) corespondingMetadata
    }

