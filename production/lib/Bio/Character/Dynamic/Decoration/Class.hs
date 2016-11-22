-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic.Decoration.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Dynamic.Decoration.Class where


import Bio.Character.Dynamic.Class
import Control.Lens


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( HasEncoded s a
      , EncodableDynamicCharacter a
      , DiscreteCharacterMetadata s a
      ) => DynamicDecoration s a | s -> a where

  
-- |
-- A decoration of a dynamic character with all direct optimization annotations.
--
-- Is a sub-class of 'DynamicDecoration'.
class ( HasFinalGapped         s a
      , HasFinalUngapped       s a
      , HasPreliminaryGapped   s a
      , HasPreliminaryUngapped s a
      , DynamicDecoration      s a
      ) => DirectOptimizationDecoration s a | s -> a where


-- |
-- A decoration of a dynamic character with the implied alignment decorations.
--
-- Is a sub-class of 'DirectOptimizationDecoration'.
class ( HasImpliedAlignment           s a
      , DirectOptimizationDecoration  s a
      ) => ImpliedAlignmentDecoration s a | s -> a where


-- |
-- A 'Lens' for the 'encoded' field
class HasEncoded s a | s -> a where

    encoded :: Lens' s a
    {-# MINIMAL encoded #-}


-- |
-- A 'Lens' for the 'finalGapped' field
class HasFinalGapped s a | s -> a where

    finalGapped :: Lens' s a
    {-# MINIMAL finalGapped #-}


-- |
-- A 'Lens' for the 'finalUngapped' field
class HasFinalUngapped s a | s -> a where

    finalUngapped :: Lens' s a
    {-# MINIMAL finalUngapped #-}


-- |
-- A 'Lens' for the 'preliminaryGapped' field
class HasPreliminaryGapped s a | s -> a where

    preliminaryGapped :: Lens' s a
    {-# MINIMAL preliminaryGapped #-}


-- |
-- A 'Lens' for the 'preliminaryUngapped' field
class HasPreliminaryUngapped s a | s -> a where

    preliminaryUngapped :: Lens' s a
    {-# MINIMAL preliminaryUngapped #-}


-- |
-- A 'Lens' for the 'impliedAlignment' field
class HasImpliedAlignment s a | s -> a where

    impliedAlignment :: Lens' s a
    {-# MINIMAL impliedAlignment #-}

