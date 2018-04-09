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

{-# LANGUAGE DeriveFoldable, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Data.NodeLabel
  ( NodeLabel()
  , fromDouble
  , fromDoubleMay
  ) where


import Control.DeepSeq
import Data.Foldable
import GHC.Generics


-- |
-- A node edge annotation for representing a possibly present user supplied
-- label.
--
-- The primary use of 'NodeLabel' is for well-typed rendering. Use the 'Show'
-- instance to nice render the 'NodeLabel'. If the user specified a label for
-- the node, that string will be returned from the call to 'show'. If the user
-- did not specify a label for the node, the Show insatnce will return
-- @{Unlabeled Node}@.
newtype NodeLabel = NL String
    deriving(Eq, Foldable, Generic, NFData, Ord)


instance Show NodeLabel where

    show (NL []) = "{Unlabeled Node}"
    show (NL xs) = xs
