------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Bin
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Bio.Metadata.Sequence.Bin
  ( StaticCharacterBin(..)
  ) where


class StaticCharacterBin b where
  binSize :: b -> Int
