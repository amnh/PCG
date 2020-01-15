-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic
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

module Bio.Character.Encodable.Dynamic
  ( AmbiguityGroup()
  , AlignmentContext(..)
  , DecodableStream(..)
  , DynamicCharacter(..)
  , DynamicCharacterElement()
  , EncodedAmbiguityGroupContainer(..)
  , EncodedGapElementContainer(..)
  , EncodableDynamicCharacter(..)
  , EncodableDynamicCharacterElement(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  , Subcomponent
  , getLeft
  , getRight
  , arbitraryDynamicCharacterOfWidth
  , renderDynamicCharacter
  ) where

import Bio.Character.Encodable.Dynamic.AmbiguityGroup
import Bio.Character.Encodable.Dynamic.Class
import Bio.Character.Encodable.Dynamic.Element
import Bio.Character.Encodable.Dynamic.Internal
import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Stream

