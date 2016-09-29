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
  ) where

import Bio.Character.Dynamic
import Bio.Character.Exportable.Class
import Bio.Character.Static
import Bio.Character.Stream
