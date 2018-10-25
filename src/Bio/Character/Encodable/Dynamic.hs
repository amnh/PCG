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
  ( DynamicCharacter(DC,Missing)
  , DynamicCharacters
  , DynamicCharacterElement()
  , EncodedAmbiguityGroupContainer(..)
  , EncodableDynamicCharacter(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  ) where

import Bio.Character.Encodable.Dynamic.Class
import Bio.Character.Encodable.Dynamic.Internal
import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Stream

