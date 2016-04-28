-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequences.Coded
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Export of coded characters
--
-----------------------------------------------------------------------------

module Bio.Sequence.Coded
  ( EncodableDynamicCharacter(..)
  , DynamicChar
  , DynamicChars
--  , encodeAll
  , decodeMany) where

import Bio.Sequence.Coded.Internal
import Bio.Sequence.Coded.Class