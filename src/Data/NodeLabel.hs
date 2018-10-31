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
  ( NodeLabel()
  ) where


import Control.DeepSeq
import Data.Default
import Data.MonoTraversable
import Data.String
import GHC.Generics


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
newtype NodeLabel = NL String
    deriving(Eq, Default, Generic, MonoFoldable, NFData, Ord)


type instance Element NodeLabel = Char


instance IsString NodeLabel where

    fromString = NL


instance Show NodeLabel where

    show (NL []) = ""
    show (NL xs) = xs
