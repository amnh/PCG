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

-- | A coded character is a character of a sequence (allows for standard chars like gap)
class CodedChar b where
    gapChar :: b