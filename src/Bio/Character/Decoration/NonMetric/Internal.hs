-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Decoration.NonMetric.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Bio.Character.Decoration.NonMetric.Internal where


import           Bio.Character.Decoration.Discrete
import           Bio.Character.Decoration.NonMetric.Class
import           Bio.Character.Encodable
import           Control.Lens


-- |
-- An abstract initial dynamic character decoration with a polymorphic character type.
newtype NonMetricDecorationInitial c
    = NonMetricDecorationInitial
    { nonMetricDecorationInitialCharacter :: c
    }


-- | (✔)
instance HasDiscreteCharacter (NonMetricDecorationInitial c) c where

    discreteCharacter = lens nonMetricDecorationInitialCharacter (\e x -> e { nonMetricDecorationInitialCharacter = x })


-- | (✔)
instance EncodableStaticCharacter c => DiscreteCharacterDecoration (NonMetricDecorationInitial c) c where


-- | (✔)
instance EncodableStaticCharacter c => SimpleDiscreteCharacterDecoration (NonMetricDecorationInitial c) c where

    toDiscreteCharacterDecoration g symbolSet =
        NonMetricDecorationInitial
        { nonMetricDecorationInitialCharacter = g symbolSet
        }


-- | (✔)
instance EncodableStaticCharacter c => NonMetricCharacterDecoration (NonMetricDecorationInitial c) c where
