{-# LANGUAGE LambdaCase #-}
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


module Data.TextShow.Custom
  ( intercalateB
  ) where

import TextShow (Builder)

intercalateB :: Builder -> [Builder] -> Builder
intercalateB sep = go sep mempty
    where
      go s a = \case
        []    -> a
        [x]   -> x <> a
        b:bs -> go s (s <> b <> a) bs
