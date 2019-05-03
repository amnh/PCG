-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TextShow.Custom
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for TextShow.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Data.TextShow.Custom
  ( intercalateB
  ) where

import Data.Foldable
import TextShow (Builder)

-- |
-- Insert the given element in between each element the list and concatenates
-- the result.
--
-- TextShow builder equivelent of 'Data.List.intercalate'.
intercalateB :: Foldable f => Builder -> f Builder -> Builder
intercalateB sep = go sep mempty . toList
    where
      go s a = \case
        []    -> a
        [x]   -> x <> a
        b:bs -> go s (s <> b <> a) bs
