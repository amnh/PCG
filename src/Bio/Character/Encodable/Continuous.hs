-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Continuous
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Bio.Character.Encodable.Continuous
  ( ContinuousChar()
  , ContinuousCharacter(..)
  , PossiblyMissingCharacter(..)
  ) where

import           Bio.Character.Encodable.Continuous.Class
import           Bio.Character.Encodable.Continuous.Internal
import           Bio.Character.Encodable.Internal            (PossiblyMissingCharacter (..))
