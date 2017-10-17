-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Shared
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Shared where


import Control.Lens
import Data.Range
import Numeric.Extended


-- |
-- Represents a character decoration that has a bounded interval for the character.
--
-- Used for Additive character scoring.
class ( HasIntervalCharacter d c
      , Ranged c
      , ExtendedNumber (Bound c)
      , Num (Finite (Bound c))
      , Num (Bound c)
      , Ord (Bound c)
      ) => RangedCharacterDecoration d c where


-- |
-- A 'Lens' for the 'isLeaf' field.
class HasIsLeaf s a | s -> a where

    {-# MINIMAL isLeaf #-}
    isLeaf :: Lens' s a


-- |
-- A 'Lens' for the 'characterCost' field.
class HasCharacterCost s a | s -> a where

    {-# MINIMAL characterCost #-}
    characterCost :: Lens' s a


-- |
-- A 'Lens' for the 'intervalCharacter' field.
class HasIntervalCharacter s a | s -> a where

    {-# MINIMAL intervalCharacter #-}
    intervalCharacter :: Lens' s a

