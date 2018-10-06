-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable
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

module Bio.Character.Encodable
  ( ContinuousChar()
  , ContinuousCharacter(..)
  , DynamicCharacter(DC,Missing)
  , DynamicCharacters
  , DynamicCharacterElement()
  , StaticCharacter()
  , StaticCharacterBlock()
  , EncodedAmbiguityGroupContainer(..)
  , EncodableDynamicCharacter(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  , PossiblyMissingCharacter(..)
  , showStream
  , showStreamElement
  ) where

import Bio.Character.Encodable.Continuous
import Bio.Character.Encodable.Dynamic
import Bio.Character.Encodable.Static
import Bio.Character.Encodable.Stream
