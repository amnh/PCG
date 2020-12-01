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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.NodeLabel
  ( NodeLabel(NL)
  , nodeLabel
  , nodeLabelString
  , nodeLabelToLazyText
  , nodeLabelToStrictText
  ) where

import           Control.DeepSeq
import           Data.Binary
import           Data.Coerce          (coerce)
import           Data.Data            (Data, Typeable)
import           Data.Default
import           Data.MonoTraversable
import           Data.String          (IsString)
import           Data.Text.Short      as TS
import           GHC.Generics
import           TextShow                (TextShow)
import           TextShow.Data.ShortText ()
import qualified Data.Text            as Strict
import qualified Data.Text.Lazy       as Lazy


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
    deriving stock    (Data, Eq, Generic, Ord, Show, Typeable)
    deriving anyclass (NFData)
    deriving newtype  (IsString, Monoid, Semigroup, TextShow)


type instance Element NodeLabel = Char


instance Default NodeLabel where

    def = mempty


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


-- |
-- Constructor for a 'NodeLabel'
nodeLabel :: ShortText -> NodeLabel
{-# INLINE nodeLabel #-}
nodeLabel = coerce


-- |
-- Construct a 'NodeLabel' from a 'String'
nodeLabelString :: String -> NodeLabel
{-# INLINE nodeLabelString #-}
nodeLabelString = coerce . fromString


{-
-- |
-- Converts a 'NodeLabel' to a 'String'.
nodeLabelToString :: NodeLabel -> String
nodeLabelToString = coerce TS.toString
-}


-- |
-- Converts a 'NodeLabel' to a strict 'Strict.Text'.
nodeLabelToStrictText :: NodeLabel -> Strict.Text
nodeLabelToStrictText = coerce TS.toText


-- |
-- Converts a 'NodeLabel' to a lazy 'Lazy.Text'.
nodeLabelToLazyText :: NodeLabel -> Lazy.Text
nodeLabelToLazyText = Lazy.fromStrict . nodeLabelToStrictText
