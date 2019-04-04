-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ShortText.Custom
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for ShortText.
--
-----------------------------------------------------------------------------


module Data.ShortText.Custom
  ( intToShortText
  ) where

import Data.Text.Short (ShortText, fromString)


-- |
-- Converts a signed integer to ShortText.
intToShortText :: Int -> ShortText
intToShortText = fromString . show

