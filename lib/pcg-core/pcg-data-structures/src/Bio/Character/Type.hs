{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Type
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module for the different character types
--
-----------------------------------------------------------------------------

module Bio.Character.Type
  ( CharacterType(..)
  ) where

import Data.Csv

-- |
-- A label for the different types of characters.
data CharacterType
  = Continuous
  | NonAdditive
  | Additive
  | Metric
  | NonMetric
  | Dynamic
  deriving Show


instance ToField CharacterType where
  toField =
    \case
      Continuous  -> "Continuous"
      NonAdditive -> "NonAdditive"
      Additive    -> "Additive"
      Metric      -> "Metric"
      NonMetric   -> "NonMetric"
      Dynamic     -> "Dynamic"
