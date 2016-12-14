-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Fitch.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Fitch.Class where


import Bio.Character.Decoration.Discrete
--import Bio.Character.Encodable
--import Bio.Metadata.Discrete
--import Control.Lens


-- |
-- An optimization class for Fith (non-additive) characters.
class DiscreteCharacterDecoration s a => FitchCharacterDecoration s a | s -> a where
