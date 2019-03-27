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

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Bio.Character.Decoration.Additive.Class where


import Bio.Character.Decoration.Shared
import Control.Lens
import Data.Range
import Numeric.Extended


-- |
-- A partially-complete character decoration that contains an interval character
-- after a post-order traversal.
class ( RangedCharacterDecoration s c
      , HasCharacterCost s (Finite (Bound c))
      , HasChildPrelimIntervals s (Range (Bound c), Range (Bound c))
      , HasIsLeaf s Bool
      , HasPreliminaryInterval s (Range (Bound c))
      ) => RangedPostorderDecoration s c | s -> c where


-- |
-- A finalized character decoration that contains an interval character after a
-- pre-order traversal.
class ( RangedCharacterDecoration s c
      , HasFinalInterval s (Range (Bound c))
      ) => RangedDecorationOptimization s c | s -> c where


-- |
-- A character decoration that can extended to represent the results of a
-- post-order traversal.
class RangedPostorderDecoration s c => RangedExtensionPostorder s c | s -> c where

    extendRangedToPostorder :: ( RangedCharacterDecoration x c
                               )
                            => x                                  -- ^ Input decoration
                            -> Finite (Bound c)                   -- ^ Local cost
                            ->  Range (Bound c)                   -- ^ Preliminary interval
                            -> (Range (Bound c), Range (Bound c)) -- ^ Child intervals
                            -> Bool                               -- ^ Is leaf node?
                            -> s


-- |
-- A character decoration that can extended to represent the results of a
-- pre-order traversal.
class ( RangedDecorationOptimization s c
      ) => RangedExtensionPreorder s c | s -> c where

    extendRangedToPreorder :: RangedPostorderDecoration x c
                           => x
                           -> Range (Bound c)
                           -> s




-- |
-- A 'Lens' for the 'additiveChildPrelimIntervals' field.
class HasChildPrelimIntervals s a | s -> a where

    {-# MINIMAL childPrelimIntervals #-}
    childPrelimIntervals :: Lens' s a


-- |
-- A 'Lens' for the 'additivePreliminaryInterval' field.
class HasPreliminaryInterval s a | s -> a where

    {-# MINIMAL preliminaryInterval #-}
    preliminaryInterval :: Lens' s a


-- |
-- A 'Lens' for the 'additiveFinalInterval' field.
class HasFinalInterval s a | s -> a where

    {-# MINIMAL finalInterval #-}
    finalInterval :: Lens' s a
