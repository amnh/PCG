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
    ContinuousChar()
  , DynamicChar()
  , DynamicChars
  , ExportableCharacterSequence(..)
  , StaticCharacter()
  , StaticCharacterBlock()
    -- * Character Classes
  , ContinuousCharacter(..)
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
import Bio.Character.Exportable.Class
