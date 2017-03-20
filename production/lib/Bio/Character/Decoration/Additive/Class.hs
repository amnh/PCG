-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Additive.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Bio.Character.Decoration.Additive.Class where


import Bio.Character.Decoration.Discrete
import Bio.Character.Decoration.Shared
import Control.Lens
import Data.Range


class ( HasStaticCharacter d c
      , Ranged c r
      , Num r
      , Ord r
      ) RangedCharacterDecoration d c where

  
class ( RangedCharacterDecoration s c
      , Ranged c r
      , HasCharacterCost s r
      , HasChildPrelimIntervals s (Range r, Range r)
      , HasIsLeaf s Bool
      , HasPreliminaryInterval s (Range r)
      ) => RangedPostOrderDecoration s c | s -> c where


class ( RangedCharacterDecoration s c
      , Ranged c r
      , HasFinalInterval s (Range r)
      ) => RangedDecorationOptimization s c | s -> c where


-- |
-- An abstract initial additive character decoration with a polymorphic character
-- type.
class DiscreteCharacterDecoration s a => AdditiveCharacterDecoration s a | s -> a where

  
class ( RangedPostOrderDecoration s c
      ) => RangedPostOrderExtention s c | s -> c where

    extendRangedToPostorder :: ( RangedCharacterDecoration x c
                               , Bound c ~ r
                               )
                            => x
                            -> r
                            -> Range r
                            -> Range r
                            -> (Range r, Range r)
                            -> Bool
                            -> s

{-
class ( RangedCharacterDecoration s c
      ) => RangedPostOrderDecoration s c | s -> c where

    extendRangedToPreorder :: ( DiscreteCharacterDecoration x c
                              , Bound c ~ r
                              )
                             => x
                             -> r
                             -> Range r
                             -> Range r
                             -> (Range r, Range r)
                             -> Bool
                             -> s
-}

-- |
-- A 'Lens' for the 'additiveChildPrelimIntervals' field.
class HasChildPrelimIntervals s a | s -> a where

    childPrelimIntervals :: Lens' s a
    {-# MINIMAL childPrelimIntervals #-}


-- |
-- A 'Lens' for the 'additivePreliminaryInterval' field.
class HasPreliminaryInterval s a | s -> a where

    preliminaryInterval :: Lens' s a
    {-# MINIMAL preliminaryInterval #-}


-- |
-- A 'Lens' for the 'additiveFinalInterval' field.
class HasFinalInterval s a | s -> a where

    finalInterval :: Lens' s a
    {-# MINIMAL finalInterval #-}
