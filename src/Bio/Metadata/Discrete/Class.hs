-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.Discrete.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Bio.Metadata.Discrete.Class
  ( HasCharacterAlphabet(..)
  , HasTcmSourceFile(..)
  ) where

import Bio.Metadata.Continuous
import Control.Lens
import Data.Text.Short         (ShortText)


-- |
-- A 'Lens' for the 'alphabet' field
class HasCharacterAlphabet s a | s -> a where

    {-# MINIMAL characterAlphabet #-}
    characterAlphabet :: Lens' s a

-- |
-- A 'Lens' for the 'tcmSourceFile' field
class HasTcmSourceFile s a | s -> a where

    {-# MINIMAL _tcmSourceFile #-}
    _tcmSourceFile :: Lens' s a


-- |
-- Note: This instance simply extracts a constant string and so doesn't obey the lens laws.
instance HasTcmSourceFile ContinuousCharacterMetadataDec ShortText where
  _tcmSourceFile = lens (const "N/A") $ \d s -> d
