-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Character.Coded
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Module holding an encoded character with certain needed values
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Sequence.Character.Coded where

import Data.Bits

-- | A coded character is a character of a sequence (allows for standard chars like gap)
-- TODO: add missing?
class Bits b => CodedChar b where
    gapChar :: Int -> b