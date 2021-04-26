-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Static
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Export of coded characters
--
-----------------------------------------------------------------------------

module Bio.Character.Encodable.Static
  ( StaticCharacter(SC)
  , StaticCharacterBlock()
  , EncodedAmbiguityGroupContainer(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  ) where

import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Static.Class
import Bio.Character.Encodable.Static.Internal
import Bio.Character.Encodable.Stream

