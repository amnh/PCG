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
  , GetSymbolChangeMatrix(..)
  , GetTransitionCostMatrix(..)
  , GetSparseTransitionCostMatrix(..)
  , HasCharacterAlphabet(..)
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  ) where

import Bio.Character.Encodable
import Bio.Metadata.Discrete
import Control.Lens


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( DiscreteCharacterMetadata     s
      , EncodableStreamElement        c
      , GetSymbolChangeMatrix         s (Word -> Word -> Word)
      , GetTransitionCostMatrix       s (c -> c -> (c, Word))
      ) => DiscreteWithTcmCharacterMetadata s c | s -> c where


-- |
-- A 'Getter' for the 'symbolChangeMatrix' field
class GetSymbolChangeMatrix s a | s -> a where

    {-# MINIMAL symbolChangeMatrix #-}
    symbolChangeMatrix :: Getter s a


-- |
-- A 'Getter' for the 'transitionCostMatrix' field
class GetTransitionCostMatrix s a | s -> a where

    {-# MINIMAL transitionCostMatrix #-}
    transitionCostMatrix  :: Getter s a


-- |
-- A 'Getter' for the 'denseTransitionCostMatrix' field
class GetSparseTransitionCostMatrix s a | s -> a where

    {-# MINIMAL sparseTransitionCostMatrix #-}
    sparseTransitionCostMatrix  :: Getter s a
