-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Dynamic
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

module Bio.Character.Dynamic
  ( DynamicChar(DC)
  , DynamicChars
  , EncodableDynamicCharacter(..)
  , EncodableStreamElement(..)
  , EncodableStream(..)
  ) where

import Bio.Character.Dynamic.Internal
import Bio.Character.Dynamic.Class
import Bio.Character.Stream

