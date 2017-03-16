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
    DynamicChar()
  , DynamicChars
  , ExportableCharacterSequence(..)
  , StaticCharacter()
  , StaticCharacterBlock()
    -- * Character Classes
  , EncodableDynamicCharacter(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStream(..)
  , EncodableStreamElement(..)
  , Exportable(..)
  , showStream
  , showStreamElement 
  ) where

import Bio.Character.Encodable
import Bio.Character.Exportable.Class
