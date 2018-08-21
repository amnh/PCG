-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.DiscreteWithTCM.Class
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

module Bio.Metadata.DiscreteWithTCM.Class
  ( DiscreteWithTcmCharacterMetadata()
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasSymbolChangeMatrix(..)
  , HasTransitionCostMatrix(..)
  , HasSparseTransitionCostMatrix(..)
  ) where

import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Control.Lens


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteCharacterMetadata     s
      , EncodableStreamElement        c
      , HasSymbolChangeMatrix         s (Word -> Word -> Word)
      , HasTransitionCostMatrix       s (c -> c -> (c, Word))
      ) => DiscreteWithTcmCharacterMetadata s c | s -> c where


-- |
-- A 'Lens' for the 'symbolChangeMatrix' field
class HasSymbolChangeMatrix s a | s -> a where

    {-# MINIMAL symbolChangeMatrix #-}
    symbolChangeMatrix :: Lens' s a


-- |
-- A 'Lens' for the 'transitionCostMatrix' field
class HasTransitionCostMatrix s a | s -> a where

    {-# MINIMAL transitionCostMatrix #-}
    transitionCostMatrix  :: Lens' s a


-- |
-- A 'Lens' for the 'denseTransitionCostMatrix' field
class HasSparseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL sparseTransitionCostMatrix #-}
    sparseTransitionCostMatrix  :: Lens' s a
