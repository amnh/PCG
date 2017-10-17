-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.Dynamic.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

-- For derived instance of PossiblyMissingCharacter
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.Character.Decoration.Dynamic.Class where


import Bio.Character.Decoration.Shared
import Bio.Character.Encodable
import Bio.Metadata.CharacterName
import Bio.Metadata.Dynamic
import Control.Lens
import Data.Alphabet
import Data.MonoTraversable


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( HasEncoded s a
      , EncodableDynamicCharacter  a
      , DynamicCharacterMetadata   s (Element a)
      ) => SimpleDynamicDecoration s a | s -> a where


-- |
-- An incomplete decoration of a dynamic character with half the direct optimization annotations.
--
-- Represents the result of just the post-order traversal.
--
-- Is a sub-class of 'DynamicCharacterDecoration'.
class ( HasCharacterCost        s Word
      , HasCharacterLocalCost   s Word
      , HasPreliminaryGapped    s a
      , HasPreliminaryUngapped  s a
      , HasLeftAlignment        s a
      , HasRightAlignment       s a
      , SimpleDynamicDecoration s a
      ) => DirectOptimizationPostOrderDecoration s a | s -> a where


-- |
-- A decoration of a dynamic character with all direct optimization annotations.
--
-- Is a sub-class of 'DirectOptimizationPreOrderDecoration'.
class ( HasFinalGapped          s a
      , HasFinalUngapped        s a
      , DirectOptimizationPostOrderDecoration s a
      ) => DirectOptimizationDecoration s a | s -> a where


-- |
-- A decoration of a dynamic character with the implied alignment decorations.
--
-- Is a sub-class of 'DirectOptimizationDecoration'.
class ( HasImpliedAlignment           s a
      , DirectOptimizationDecoration  s a
      ) => ImpliedAlignmentDecoration s a | s -> a where


-- |
-- A decoration of an initial encoding of a dynamic character which has the
-- appropriate 'Lens' & character class constraints.
class ( SimpleDynamicDecoration s a
      ) => DynamicCharacterDecoration s a | s -> a where

    toDynamicCharacterDecoration :: CharacterName -> Double -> Alphabet String -> (Word -> Word -> Word) -> (x -> a) -> x -> s
    {-# MINIMAL toDynamicCharacterDecoration #-}


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( SimpleDynamicDecoration s c
      , DirectOptimizationPostOrderDecoration s c
      ) => SimpleDynamicExtensionPostOrderDecoration s c | s -> c where

    extendDynamicToPostOrder :: SimpleDynamicDecoration x c
                             => x    -- ^ Original decoration
                             -> Word -- ^ The cost of the alignment
                             -> Word -- ^ The cost of the alignment and the child subtrees
                             -> c    -- ^ Preliminary /ungapped/ dynamic character
                             -> c    -- ^ Preliminary   /gapped/ dynamic character
                             -> c    -- ^ Left  alignment dynamic character
                             -> c    -- ^ Right alignment dynamic character
                             -> s    -- ^ Resulting decoration


-- |
-- A decoration that can be constructed from a 'DiscreteCharacterDecoration' by
-- extending the decoration to contain the requisite fields for performing
-- Sankoff's algorithm.
class ( DirectOptimizationPostOrderDecoration s c
      , DirectOptimizationDecoration s c
      ) => PostOrderExtensionDirectOptimizationDecoration s c | s -> c where

    extendPostOrderToDirectOptimization :: DirectOptimizationPostOrderDecoration x c
                                        => x -- ^ Original decoration
                                        -> c -- ^ Final /ungapped/ dynamic character
                                        -> c -- ^ Final   /gapped/ dynamic character
                                        -> s -- ^ Resulting decoration

{-
instance ( DynamicCharacterDecoration s a
         , PossiblyMissingCharacter a
         ) => PossiblyMissingCharacter s where

    isMissing = isMissing . (^. encoded)

    toMissing x = x & encoded %~ toMissing 
-}


-- |
-- A 'Lens' for the 'encoded' field
class HasCharacterLocalCost s a | s -> a where

    characterLocalCost :: Lens' s a
    {-# MINIMAL characterLocalCost #-}


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
-- A 'Lens' for the 'leftAlignment' field
class HasLeftAlignment s a | s -> a where

    leftAlignment :: Lens' s a
    {-# MINIMAL leftAlignment #-}


-- |
-- A 'Lens' for the 'rightAlignment' field
class HasRightAlignment s a | s -> a where

    rightAlignment :: Lens' s a
    {-# MINIMAL rightAlignment #-}


-- |
-- A 'Lens' for the 'impliedAlignment' field
class HasImpliedAlignment s a | s -> a where

    impliedAlignment :: Lens' s a
    {-# MINIMAL impliedAlignment #-}

