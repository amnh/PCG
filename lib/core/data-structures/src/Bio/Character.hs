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
    AmbiguityGroup
  , ContinuousCharacter()
  , DynamicCharacter()
  , DynamicCharacterElement()
  , ExportableCharacterBuffer(..)
  , StaticCharacter()
  , StaticCharacterBlock()
  , Subcomponent
    -- * Character Classes
  , DecodableStream(..)
  , EncodedAmbiguityGroupContainer(..)
  , EncodableContinuousCharacter(..)
  , EncodableDynamicCharacter(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStream(..)
  , EncodableStreamElement(..)
  , ExportableBuffer(..)
  , ExportableElements(..)
  , PossiblyMissingCharacter(..)
  , showStream
  , showStreamElement
  ) where

import Bio.Character.Encodable
import Bio.Character.Exportable
