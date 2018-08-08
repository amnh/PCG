-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MutualExclusionSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Set-like structure for a collection of elements where each included element
-- implies the exclusion of another element.
--
-- The 'MutualExclusionSet' holds a /bijective/ mapping between a set of included
-- elements and their corresponding mutually exclusive element. The construction
-- allows for efficient access to included and excluded elements.
--
-- The following properties will always hold:
--
--  * @ invert . invert === id @
--
--  * @ includedSet === excludedSet . invert @
--
--  * @ excludedSet === includedSet . invert @
--
--  * @ toList === toList . includedSet @
--
--  * @ toList . invert === toList . excludedSet  @
--
--  * @ ∀ e, isIncluded e ==> not . isExcluded e @
--
--  * @ ∀ e, isExcluded e ==> not . isIncluded e @
--
--  * @ ∀ e, isIncluded e === isExcluded e . invert @
--
--  * @ ∀ e, isExcluded e === isIncluded e . invert @
--
--  * @ ∀ e, ∃! k, isIncluded e ==> excludedLookup k == Just e @
--
--  * @ ∀ e, ∃! k, isExcluded e ==> includedLookup k == Just e @
--
-----------------------------------------------------------------------------

module Data.MutualExclusionSet
  ( MutualExclusionSet
  -- * Construction
  , singleton
  , unsafeFromList
  -- * Deconstruction
  , excludedSet
  , includedSet
  , mutuallyExclusivePairs
  -- * Manipulation
  , invert
  , merge
  , prettyPrintMutualExclusionSet
  -- * Lookup
  , excludedLookup
  , includedLookup
  -- * Relational query
  , isExcluded
  , isIncluded
  -- * Holistic query
  , isPermissible
  , isCoherent
  ) where

import           Data.MutualExclusionSet.Internal
