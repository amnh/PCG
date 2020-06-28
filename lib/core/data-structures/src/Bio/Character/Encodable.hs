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
  ( AmbiguityGroup()
  , AlignmentContext(..)
  , ContinuousCharacter()
  , DecodableStream(..)
  , DynamicCharacter(..)
  , DynamicCharacterElement()
  , StaticCharacter()
  , StaticCharacterBlock()
  , EncodedAmbiguityGroupContainer(..)
  , EncodableContinuousCharacter(..)
  , EncodableDynamicCharacter(..)
  , EncodableDynamicCharacterElement(..)
  , EncodableStaticCharacter(..)
  , EncodableStaticCharacterStream(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  , PossiblyMissingCharacter(..)
  , Subcomponent
  , renderDynamicCharacter
  , showStream
  , showStreamElement
--  , selectDC
  ) where

import Bio.Character.Encodable.Continuous
import Bio.Character.Encodable.Dynamic
import Bio.Character.Encodable.Static
import Bio.Character.Encodable.Stream
