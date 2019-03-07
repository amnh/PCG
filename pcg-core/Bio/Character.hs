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
-----------------------------------------------------------------------------

module Bio.Character
  (  -- * Character Types
    ContinuousCharacter()
  , DynamicCharacter()
  , DynamicCharacterElement()
  , DynamicCharacters
  , ExportableCharacterSequence(..)
  , StaticCharacter()
  , StaticCharacterBlock()
    -- * Character Classes
  , EncodableContinuousCharacter(..)
  , EncodableDynamicCharacter(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStream(..)
  , EncodableStreamElement(..)
  , Exportable(..)
  , PossiblyMissingCharacter(..)
  , showStream
  , showStreamElement
  ) where

import Bio.Character.Encodable
import Bio.Character.Exportable
