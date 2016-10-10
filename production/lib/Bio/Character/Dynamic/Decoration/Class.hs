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


class ( HasEncoded s a
      , EncodableDynamicCharacter a
      ) => DynamicDecoration s a | s -> a where

  
class ( HasFinalGapped         s a
      , HasFinalUngapped       s a
      , HasPreliminaryGapped   s a
      , HasPreliminaryUngapped s a
      , DynamicDecoration      s a
      ) => DirectOptimizationDecoration s a | s -> a where


class ( HasImpliedAlignment           s a
      , DirectOptimizationDecoration  s a
      ) => ImpliedAlignmentDecoration s a | s -> a where


class HasEncoded s a | s -> a where

    encoded :: Lens' s a
    {-# MINIMAL encoded #-}


class HasFinalGapped s a | s -> a where

    finalGapped :: Lens' s a
    {-# MINIMAL finalGapped #-}


class HasFinalUngapped s a | s -> a where

    finalUngapped :: Lens' s a
    {-# MINIMAL finalUngapped #-}


class HasPreliminaryGapped s a | s -> a where

    preliminaryGapped :: Lens' s a
    {-# MINIMAL preliminaryGapped #-}


class HasPreliminaryUngapped s a | s -> a where

    preliminaryUngapped :: Lens' s a
    {-# MINIMAL preliminaryUngapped #-}


class HasImpliedAlignment s a | s -> a where

    impliedAlignment :: Lens' s a
    {-# MINIMAL impliedAlignment #-}

