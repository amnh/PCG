-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Static
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

module Bio.Character.Static
  ( StaticCharacter()
  , StaticCharacterBlock()
  , EncodedAmbiguityGroupContainer(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  ) where

import Bio.Character.Internal
import Bio.Character.Static.Internal
import Bio.Character.Static.Class
import Bio.Character.Stream

