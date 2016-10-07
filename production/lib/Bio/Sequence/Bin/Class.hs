------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Sequence.Bin.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Sequence.Bin.Class
  ( StaticCharacterBin(..)
  , EncodedAmbiguityGroupContainer(..)
  ) where


import Bio.Character.Internal


class EncodedAmbiguityGroupContainer b => StaticCharacterBin b where

    binSize :: b -> Int
