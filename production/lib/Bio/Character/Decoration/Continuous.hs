-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Continuous.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Decoration.Continuous
  ( ContinuousDecorationInitial()
  , ContinuousChar()
  , ContinuousCharacter()
  , GeneralCharacterMetadata()
  , HasCharacterName(..)
  , HasCharacterWeight(..)
  , HasContinuousCharacter(..)
  , continuousDecorationInitial
  ) where


import Bio.Character.Decoration.Continuous.Class
import Bio.Character.Decoration.Continuous.Internal
import Bio.Metadata.General

