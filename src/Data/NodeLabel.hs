-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NodeLabel
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A possible present, stringly-typed label for nodes with a nice Show instance.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.NodeLabel
  ( NodeLabel(NL)
  , nodeLabel
  , nodeLabelString
  ) where


import Control.DeepSeq
import Data.Default
import Data.MonoTraversable
import GHC.Generics
import Data.Text.Short as TS
import Data.Binary
import Data.String (IsString())


-- |
-- A node edge annotation for representing a possibly present user supplied
-- label.
--
-- Use 'IsString' instance to construct a user supplied 'NodeLabel'.
--
-- Use 'Default' instance to construct the empty label.
--
-- The primary use of 'NodeLabel' is for well-typed rendering. Use the 'Show'
-- instance to nicely render the 'NodeLabel'. If the user specified a label for
-- the node, that string will be returned from the call to 'show'. If the user
-- did not specify a label for the node, the Show insatnce will return
-- @{Unlabeled Node}@.
newtype NodeLabel = NL {getNodeLabel :: ShortText}
    deriving(Eq, Generic, IsString, Monoid, NFData, Ord, Semigroup, Show)

-- |
-- Constructor for a 'NodeLabel'
nodeLabel :: ShortText -> NodeLabel
nodeLabel = NL

-- |
-- Construct a 'NodeLabel' from a 'String'
nodeLabelString :: String -> NodeLabel
nodeLabelString = NL. fromString


instance Default NodeLabel where
  def = mempty

type instance Element NodeLabel = Char

instance Binary NodeLabel


instance MonoFoldable NodeLabel where

    -- |
    -- Map each element of a structure to a 'Monoid' and combine the results.
    {-# INLINE ofoldMap #-}
    ofoldMap f = TS.foldr (mappend . f) mempty . getNodeLabel

    -- |
    -- Right-associative fold of a structure.
    {-# INLINE ofoldr #-}
    ofoldr f e = TS.foldr f e . getNodeLabel

    -- |
    -- Strict left-associative fold of a structure.
    {-# INLINE ofoldl' #-}
    ofoldl' f e = TS.foldl' f e . getNodeLabel


    -- |
    -- This function throws an error and is only defined for the typeclass definition.
    ofoldr1Ex = error "Do not use ofoldr1Ex on NodeLabel"

    -- |
    -- This function throws an error and is only here for the typeclass definition.
    ofoldl1Ex' = error "Do not use ofoldl1Ex' on NodeLabel"

    
-- |
-- Performs a element-wise monomporphic map over a 'NodeLabel'.
instance MonoFunctor NodeLabel where

  omap f (NL shortText) = NL $ TS.foldr g mempty shortText

    where
      g char acc = singleton (f char) <> acc
    

