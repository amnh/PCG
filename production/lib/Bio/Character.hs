-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character
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

module Bio.Character
  ( DynamicChar()
  , DynamicChars
  , EncodableDynamicCharacter(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStream(..)
  , EncodableStreamElement(..)
  , Exportable(..)
  , ExportableCharacterSequence(characterCount, characterWidth, bufferChunks)
  , StaticCharacter()
  , StaticCharacterBlock()
  ) where

import Bio.Character.Dynamic
import Bio.Character.Exportable.Class
import Bio.Character.Static
import Bio.Character.Stream
